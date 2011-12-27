-module(sockjs_test).
-export([start/0]).

start() ->
    Port = 8080,
    application:start(sockjs),
    application:start(gproc),
    {ok, _} = misultin:start_link([{loop,        fun handle_http/1},
				   {ws_loop,     fun handle_ws/1},
				   {ws_autoexit, false},
				   {port,        Port}]),
    io:format("~nRunning on port ~p~n~n", [Port]),
    broadcast_loop(start),
    receive
        _ -> ok
    end.

%% --------------------------------------------------------------------------
handle_http(Req) ->
    Path = Req:resource([lowercase, urldecode]),
    try
	case sockjs:handler(Req,Path,fun handle_sockjs/1) of
	    nomatch ->
		case handle(Path) of
		    Fun when is_function(Fun) ->
			Fun(Req);
		    nomatch ->
			Req:respond(404,"Error 404")
		end;
	    _ -> ok
	end
    catch
	_ ->
	    Req:respond(500,"Error")
    end.

handle_ws(Ws) ->
    case lists:reverse(resource([lowercase, urldecode],Ws)) of
	["websocket",_Session,_Server | Rest] ->
	    case handle_sockjs(lists:reverse(Rest)) of
		SFun when is_function(SFun) ->
		    sockjs:ws_loop(Ws,SFun);
		nomatch ->
		    closed
	    end;
	_ ->
	    closed
    end.
%% --------------------------------------------------------------------------


handle(["config.js"]) -> fun config_js/1;
handle(_) -> fun file/1.

handle_sockjs(["echo"]) -> fun echo_loop/1;
handle_sockjs(["close"]) -> fun close_loop/1;
handle_sockjs(["amplify"]) -> fun amplify_loop/1;
handle_sockjs(["broadcast"]) -> fun broadcast_loop/1;
handle_sockjs(_) -> nomatch.

file(Req) ->
    %% TODO unsaf
    Path = case Req:resource([]) of
	       [] ->
		   "index.html";
	       Else ->
		   filename:join(Else)
	   end,
    LocalPath = filename:join([module_path(), "priv/www", Path]),
    Req:file(LocalPath).

module_path() ->
    {file, Here} = code:is_loaded(?MODULE),
    filename:dirname(filename:dirname(Here)).

config_js(Req) ->
    %% TODO parse the file? Good luck, it's JS not JSON.
    Req:ok([{"content-type", "application/javascript"}],
	   "var client_opts = {\"url\":\"http://\"+window.location.host+\"\",\"disabled_transports\":[],\"sockjs_opts\":{\"devel\":true}};").

%% --------------------------------------------------------------------------

echo_loop(Conn) ->
    receive
	{browser,Data} ->
	    Conn:send(Data),
	    echo_loop(Conn);
	_ ->
	    ok
    end.

close_loop(Conn) ->
    Conn:close(3000, "Go away!").
	    

amplify_loop(Conn) ->
    receive
	{browser,Data} ->
	    N0 = list_to_integer(binary_to_list(Data)),
	    N = if N0 > 0 andalso N0 < 19 -> N0;
		   true                   -> 1
		end,
	    Conn:send(list_to_binary(string:copies("x", round(math:pow(2, N))))),
	    amplify_loop(Conn);
	_ ->
	    ok
    end.

broadcast_loop(start) ->
    ets:new(broadcast_table, [public, named_table]);
broadcast_loop(Conn) ->
    ets:insert(broadcast_table, {Conn}),
    Loop = fun(Loop) ->
		   receive
		       {browser,Data} ->
			   ets:foldl(fun({C}, _Acc) -> C:send(Data) end, [], broadcast_table),
			   Loop(Loop);
		       closed ->
			   ets:delete_object(broadcast_table, {Conn})
		   end
	   end,
    Loop(Loop).

resource(Options, ReqT) when is_list(Options) ->
    RawUri = ReqT:get(path),
    Uri = lists:foldl(fun(Option, Acc) -> clean_uri(Option, Acc) end, RawUri, Options),
    string:tokens(Uri, "/").

-spec clean_uri(Option::atom(), Uri::string()) -> string().
clean_uri(lowercase, Uri) ->
	string:to_lower(Uri);
clean_uri(urldecode, Uri) ->
	misultin_utility:unquote(Uri);
clean_uri(_Unavailable, Uri) ->
	Uri.
