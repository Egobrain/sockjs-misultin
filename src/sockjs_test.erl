-module(sockjs_test).
-export([start/0]).

start() ->
    Port = 8080,
    application:start(sockjs),
    application:start(gproc),
    {ok, _} = misultin:start_link([{loop,        sockjs:http_handler(fun handle/1, fun handle_sockjs/1)},
				   {ws_loop,     sockjs:ws_handler(fun handle_sockjs/1)},
				   {ws_autoexit, false},
				   {port,        Port}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    broadcast_loop(start),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------

handle([]) -> fun index/1;
handle(["config.js"]) -> fun config_js/1;
handle(_) -> fun file/1.

handle_sockjs(["echo"]) -> fun echo_loop/1;
handle_sockjs(["close"]) -> fun close_loop/1;
handle_sockjs(["amplify"]) -> fun amplify_loop/1;
handle_sockjs(["broadcast"]) -> fun broadcast_loop/1;
handle_sockjs(_) -> nomatch.


index(Req) ->
    Req:file("priv/www/index.html").
file(Req) ->
    LocalPath = filename:join([module_path(), "priv/www", filename:join(Req:resource([]))]),
    Req:file(LocalPath).

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

config_js(Req) ->
    %% TODO parse the file? Good luck, it's JS not JSON.
    Req:ok([{"content-type", "application/javascript"}],
	   "var client_opts = {\"url\":\"http://\"+window.location.host+\"\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};").

%% --------------------------------------------------------------------------

echo_loop(Ws) ->
    receive
	{browser,JSON} ->
	    case sockjs_util:decode(JSON) of
		{ok, Data} ->
		    io:format("session: ~p",[Ws:session()]),
		    Ws:send(Data),
		    echo_loop(Ws);
		{error,_} ->
		    Ws:close(500,"Invalid JSON")
	    end;
	_ ->
	    ok
    end.

close_loop(Conn) ->
    Conn:close(3000, "Go away!").
	    

amplify_loop(Ws) ->
    receive
	{browser,JSON} ->
	    case sockjs_util:decode(JSON) of
		{ok, Data} ->
		    N0 = list_to_integer(binary_to_list(Data)),
		    N = if N0 > 0 andalso N0 < 19 -> N0;
			   true                   -> 1
			end,
		    Ws:send(list_to_binary(string:copies("x", round(math:pow(2, N))))),
		    amplify_loop(Ws);
		{error,_} ->
		    Ws:close(500,"Invalid JSON")
	    end;
	
	_ ->
	    ok
    end.

broadcast_loop(start) ->
    ets:new(broadcast_table, [public, named_table]);
broadcast_loop(Ws) ->
    ets:insert(broadcast_table, {Ws}),
    Loop = fun(Loop) ->
		   receive
		       {browser,JSON} ->
			   case sockjs_util:decode(JSON) of
			       {ok, Data} ->
				   ets:foldl(fun({C}, _Acc) -> C:send(Data) end, [], broadcast_table),
				   Loop(Loop);
			       {error,_} ->
				   Ws:close(500,"Invalid JSON")
			   end;
		       closed ->
			   ets:delete_object(broadcast_table, {Ws})
		   end
	   end,
    Loop(Loop).
