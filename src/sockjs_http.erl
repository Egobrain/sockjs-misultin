-module(sockjs_http).

-export([path/1, method/1, body/1, body_qs/1, jsessionid/1, callback/1,
         header/2, reply/4, chunk_start/3, chunk/2, chunk_end/1]).

-export([misultin_ws_loop/2]).

%% --------------------------------------------------------------------------

path(Req) -> case element(1, Req) of
                                 misultin_ws -> Req:get(path);
                                 _           -> {abs_path, Path} = Req:get(uri),Path
                             end.

method(Req) -> Req:get(method).

body(Req) -> Req:get(body).

body_qs(Req) ->
    case header('Content-Type', Req) of
        "text/plain" ->
            body(Req);
        _ ->
            %% Assume application/x-www-form-urlencoded by default
            proplists:get_value("d", Req:parse_post())
    end.


%% TODO fix Req mutation for these two
jsessionid(Req) ->
    Req:get_cookie_value("JSESSIONID", Req:get_cookies()).

callback(Req) ->
    case proplists:get_value("c", Req:parse_qs()) of
        undefined ->
            undefined;
        CB ->
            list_to_binary(CB)
    end.

header(K, Req) ->
    case misultin_utility:header_get_value(K, Req:get(headers)) of
        false -> undefined;
        V -> V
    end.

reply(Code, Headers, Body, Req) ->
    Req:respond(Code, Headers, Body),
    Req.

chunk_start(_Code, Headers, Req) ->
    Req:chunk(head, Headers),
    Req.

chunk(Chunk, Req) -> case Req:chunk(Chunk) of
			 {stream_data, _} -> ok
%% Misultin just kills the
%% process on connection error.
		     end.

chunk_end(Req) -> Req:chunk(done),
		  Req.

% enbinary(L) -> [{list_to_binary(K), list_to_binary(V)} || {K, V} <- L].

%% --------------------------------------------------------------------------

-define(WS_MODULE, sockjs_ws).

misultin_ws_loop(Ws, Receive) ->
    io:format("LOG-3~n",[]),
    Ws:send(["o"]),
    Self = {?WS_MODULE, Ws},
    io:format("LOG-4~n",[]),
    Receive(Self, init),
    io:format("LOG-5~n",[]),
    misultin_ws_loop0(Ws, Receive, Self),
    io:format("LOG-6~n",[]).

misultin_ws_loop0(Ws, Receive, Self) ->
    Msg = receive
	      Msg1 -> Msg1
	  end,
    case Msg of 
        {browser, ""} ->
	    io:format("WS LOG 1~n",[]),
            misultin_ws_loop0(Ws, Receive, Self);
        {browser, Data} ->
	    io:format("WS LOG 2~n",[]),
            case sockjs_util:decode(Data) of
                {ok, Decoded} ->
		    io:format("WS LOG 2-1~n",[]),
                    Receive(Self, {recv, Decoded}),
                    misultin_ws_loop0(Ws, Receive, Self);
                {error, REASON} ->
		    io:format("WS LOG 2-2: ~p~n ",[REASON]),
		    Self:close(500,"Broken JSON encoding.'"),
                    closed
            end;
        closed ->
	    io:format("WS LOG 3~n",[]),
            closed;
        Msg ->
	    io:format("WS LOG 4~n",[]),
            Receive(Self, {info, Msg}),
            misultin_ws_loop0(Ws, Receive, Self)
    end.

