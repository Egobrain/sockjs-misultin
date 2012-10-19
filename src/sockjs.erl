-module(sockjs).

-export([http_handler/2,ws_handler/1]).
-export([ws_loop/2]).

-define(STILL_OPEN, {2010, "Another connection still open"}).

-define(IFRAME, "<!DOCTYPE html>
<html>
<head>
  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
  <script>
    document.domain = document.domain;
    _sockjs_onload = function(){SockJS.bootstrap_iframe();};
  </script>
  <script src=\"~s\"></script>
</head>
<body>
  <h2>Don't panic!</h2>
  <p>This is a SockJS hidden iframe. It's used for cross domain magic.</p>
</body>
</html>").

-define(IFRAME_HTMLFILE, "<!doctype html>
<html><head>
  <meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\" />
  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />
</head><body><h2>Don't panic!</h2>
  <script>
    document.domain = document.domain;
    var c = parent.~s;
    c.start();
    function p(d) {c.message(d);};
    window.onload = function() {c.stop();};
  </script>").

http_handler(HttpHandler,SockJSHandler) when is_function(HttpHandler) andalso is_function(SockJSHandler) ->
    fun(Req) ->
	    Path = Req:resource([lowercase, urldecode]),
	    try
		case handler(Req,Path,SockJSHandler) of
		    nomatch ->
			case HttpHandler(Path) of
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
	    end
    end.

ws_handler(SockJSHandler) ->
    fun(Ws) ->
	    case lists:reverse(resource([lowercase, urldecode],Ws)) of
		["websocket",_Session,_Server | Rest] ->
		    case SockJSHandler(lists:reverse(Rest)) of
			SFun when is_function(SFun) ->
			    sockjs:ws_loop(Ws,SFun);
			nomatch ->
			    closed
		    end;
		_ ->
		    closed
	    end
    end.



handler(Req,Path,Handle) ->
    Method = Req:get(method),
    case handle(Method,lists:reverse(Path)) of
	nomatch ->
	    nomatch;
	{RestPath,Type,Server,Session,Fun,HFun} ->
	    case Handle(lists:reverse(RestPath)) of
		Loop when is_function(Loop) ->
		    Headers = HFun(Req,[]),
		    case Type of
			send ->
			    sockjs_session:maybe_create(Session,Loop),
			    Fun(Req,Headers,Server,Session);
			recv ->
			    try
				Fun(Req,Headers,Session,Session)
			    catch
				throw:no_session ->
				    Req:respond(404,h_sid(Req,[]),"")
			    end
		    end;
		nomatch ->
		    nomatch
	    end
    end.

-spec handle(Method::atom(),RevURL::list(string())) -> 
		    {RestRevURL::list(string()),(send | recv),Session::string(),Server::string(),Handle::function(),Header::function()}.
handle('GET',    ["websocket",Session,Serv|Rest]) ->
    {Rest,send,Session,Serv};
handle('POST',   ["xhr_send",Session,Serv|Rest]) ->
    {Rest,recv, Serv,Session, fun xhr_send/4,      compose([fun h_sid/2, fun xhr_cors/2,fun expect_xhr/2])};
handle('OPTIONS',["xhr_send",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun options/4,       compose([fun h_sid/2, fun xhr_cors/2, fun cache_for/2, fun xhr_options/2])};
handle('POST',   ["xhr",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun xhr_polling/4,   compose([fun h_sid/2, fun xhr_cors/2])};
handle('OPTIONS',["xhr",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun options/4,       compose([fun h_sid/2, fun xhr_cors/2, fun cache_for/2, fun xhr_options/2])};
handle('POST',   ["xhr_streaming",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun xhr_streaming/4, compose([fun h_sid/2, fun xhr_cors/2])};
handle('OPTIONS',["xhr_streaming",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun options/4,       compose([fun h_sid/2, fun xhr_cors/2, fun cache_for/2, fun xhr_options/2])};
handle('POST',   ["jsonp_send",Session,Serv|Rest]) ->
    {Rest,recv, Serv,Session, fun jsonp_send/4,    compose([fun h_sid/2, fun expect_form/2])};
handle('GET',    ["jsonp",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun jsonp/4,         compose([fun h_sid/2, fun h_no_cache/2])};
handle('GET',    ["eventsource",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun eventsource/4,   compose([fun h_sid/2, fun h_no_cache/2])};
handle('GET',    ["htmlfile",Session,Serv|Rest]) ->
    {Rest,send, Serv,Session, fun htmlfile/4,      compose([fun h_sid/2, fun h_no_cache/2])};
handle('POST',   ["chunking_test"|Rest]) ->
    {Rest,send, dummy, dummy, fun chunking_test/4, compose([fun xhr_cors/2, fun expect_xhr/2])};
handle('OPTIONS',["chunking_test"|Rest]) ->
    {Rest,send, dummy, dummy, fun options/4,       compose([fun h_sid/2, fun xhr_cors/2, fun cache_for/2, fun xhr_options/2])};
handle('GET',    ["iframe"++_|Rest]) ->
    {Rest,send, dummy, dummy, fun iframe/4,        compose([fun cache_for/2])};
handle(_,_) ->  nomatch.


%% --------------------------------------------------------------------------

h_sid(Req, Headers) ->
    case Req:get_cookie_value("JSESSIONID", Req:get_cookies()) of
        undefined -> [{"Set-Cookie", "JSESSIONID=dummy; path=/"}];
        Jsid      -> [{"Set-Cookie", "JSESSIONID=" ++ Jsid ++ "; path=/"}]
    end ++ Headers.

h_no_cache(_Req, Headers) ->
    [{"Cache-Control", "no-store, no-cache, must-revalidate, max-age=0"}] ++
        Headers.

xhr_cors(Req, Headers) ->
    Origin = case misultin_utility:header_get_value('Origin', Req:get(headers)) of
                 false -> "*";
                 O     -> O
             end,
    AllowHeaders = case misultin_utility:header_get_value('Access-Control-Request-Headers', Req:get(headers)) of
                       false -> [];
                       V     -> [{"Access-Control-Allow-Headers", V}]
                   end,
    [{"Access-Control-Allow-Origin",      Origin},
     {"Access-Control-Allow-Credentials", "true"}] ++ AllowHeaders ++ Headers.

xhr_options(_Req, Headers) ->
    [{"Allow",                  "OPTIONS, POST"},
     {"Access-Control-Max-Age", "31536000"}] ++ Headers. % year = 365*24*60*60

cache_for(_Req, Headers) ->
    {{Y,M,D},Time} = calendar:local_time(),
    [{"Cache-Control", "public, max-age=31536000"}, % year = 365*24*60*60
     {"Expires",       httpd_util:rfc1123_date({{Y+1,M,D},Time})}] ++ Headers.

compose(Funs) ->
    fun(Req,Header) ->
	    lists:foldl(fun(Fun,Result) -> Fun(Req,Result) end,Header,Funs)
    end.

expect_xhr(_Req,Headers) ->
    % todo
    Headers.

expect_form(_Req,Headers) ->
    % todo
    Headers.

%% --------------------------------------------------------------------------


xhr_polling(Req, Headers, _Server, SessionId) ->
    Req:options([{comet, true}]),
    headers(Req, Headers),
    reply_loop(Req, SessionId, true, fun fmt_xhr/1).

xhr_streaming(Req, Headers, _Server, SessionId) ->
    Req:options([{comet, true}]),
    headers(Req, Headers),
    %% IE requires 2KB prefix:
    %% http://blogs.msdn.com/b/ieinternals/archive/2010/04/06/comet-streaming-in-internet-explorer-with-xmlhttprequest-and-xdomainrequest.aspx
    chunk(Req, list_to_binary(string:copies("h", 2048)), fun fmt_xhr/1),
    reply_loop(Req, SessionId, false, fun fmt_xhr/1).

jsonp(Req, Headers, _Server, SessionId) ->
    Req:options([{comet, true}]),
    S = fun (CB) ->
		headers(Req, Headers),
		reply_loop(Req, SessionId, true,
			   fun (Body) -> fmt_jsonp(Body, CB) end)
	 end,
    verify_callback(Req, S).



verify_callback(Req, Success) ->
    Req:options([{comet, true}]),
    case proplists:get_value("c", Req:parse_qs()) of
        undefined ->
            Req:respond(500, [], "\"callback\" parameter required");
        CB ->
	    Success(list_to_binary(CB))
    end.

iframe(Req, Headers, _Server, _SessionId) ->
    Req:options([{comet, true}]),
    {ok, URL} = application:get_env(sockjs, sockjs_url),
    IFrame = fmt(?IFRAME, [URL]),
    MD5 = "\"" ++ binary_to_list(base64:encode(erlang:md5(IFrame))) ++ "\"",
    case misultin_utility:header_get_value('If-None-Match', Req:get(headers)) of
        MD5 ->
	    Req:respond(304, [], "");
        _   -> Req:respond(
                 200, [{"Content-Type", "text/html; charset=UTF-8"},
                       {"ETag",         MD5}] ++ Headers, IFrame)
    end.

eventsource(Req, Headers, _Server, SessionId) ->
    Req:options([{comet, true}]),
    headers(Req, Headers, "text/event-stream; charset=UTF-8"),
    chunk(Req, <<$\r, $\n>>),
    reply_loop(Req, SessionId, false, fun fmt_eventsource/1).

reply_loop(Req, SessionId, Once, Fmt) ->
    {ok, Heartbeat} = application:get_env(sockjs, heartbeat_ms),
    case sockjs_session:reply(SessionId, Once) of
        wait ->
	    receive
		go -> reply_loop(Req, SessionId, Once, Fmt)
	    after Heartbeat ->
		    chunk(Req, <<"h">>, Fmt),
		    reply_loop0(Req, SessionId, Once, Fmt)
	    end;
        session_in_use ->
	    Err = sockjs_util:encode_list([{close, ?STILL_OPEN}]),
	    chunk(Req, Err, Fmt),
	    Req:chunk(done);
        Reply ->
	    chunk(Req, Reply, Fmt),
	    reply_loop0(Req, SessionId, Once, Fmt)
    end.

reply_loop0(Req, _SessionId, true, _Fmt) ->
    Req:chunk(done);
reply_loop0(Req, SessionId, false, Fmt) ->
    reply_loop(Req, SessionId, false, Fmt).

htmlfile(Req, Headers, _Server, SessionId) ->
    Req:options([{comet, true}]),
    S = fun (CB) ->
                headers(Req, Headers, "text/html; charset=UTF-8"),
                IFrame0 = fmt(?IFRAME_HTMLFILE, [CB]),
                %% Safari needs at least 1024 bytes to parse the
                %% website. Relevant:
                %%   http://code.google.com/p/browsersec/wiki/Part2#Survey_of_content_sniffing_behaviors
                Padding = list_to_binary(string:copies(" ", 1024 - size(IFrame0))),
                IFrame = <<IFrame0/binary, Padding/binary, $\r, $\n, $\r, $\n>>,
                chunk(Req, IFrame),
                reply_loop(Req, SessionId, false, fun fmt_htmlfile/1)
        end,
    verify_callback(Req, S).

chunking_test(Req, Headers, _Server, _SessionId) ->
    Req:options([{comet,true}]),
    headers(Req,Headers),
    Prelude = list_to_binary(string:copies(" ", 2048)),
    chunking_loop(Req, [{0,    <<"h">>},
			{0,    <<Prelude/binary, "h">>},
			{5,    <<"h">>},
			{25,   <<"h">>},
			{125,  <<"h">>},
			{625,  <<"h">>},
			{3125, <<"h">>}]).

chunking_loop(Req,  []) ->
    Req:chunk(done);
chunking_loop(Req, [{Timeout, Payload} | Rest]) ->
    timer:sleep(Timeout),
    R = chunk(Req, Payload, fun fmt_xhr/1),
    Rest1 = case R of
                {stream_data,_} -> Rest;
                _ -> []
            end,
    chunking_loop(Req, Rest1).
							 
options(Req, Headers, _Server, _SessionId) ->
    Req:options([{comet, true}]),
    Req:respond(204, Headers, "").

%% --------------------------------------------------------------------------
%% This is send but it receives - "send" from the client POV, receive
%% from ours.

xhr_send(Req, Headers, _Server, SessionId) ->
    Body = Req:get(body),
    Success = fun () -> Req:respond(204,[{"content-type", "text/plain"}] ++ Headers, "") end,
    verify_body(Req, Body, SessionId, Success).

jsonp_send(Req, Headers, _Server, SessionId) ->
    Body = case misultin_utility:header_get_value('Content-Type', Req:get(headers)) of
	       "text/plain" ->
		   Req:get(body);
	       _ -> %% Assume application/x-www-form-urlencoded by default
		   proplists:get_value("d", Req:parse_post())
	   end,
    Success = fun () -> Req:respond(200, Headers, "ok") end,
    verify_body(Req, Body, SessionId, Success).

verify_body(Req, Body, _SessionId, _Success)
   when Body =:= undefined orelse Body =:= [] orelse Body =:= <<>> ->
     sockjs_http:reply(500, [], "Payload expected.", Req);

verify_body(Req, Body, SessionId, Success) ->
    case sockjs_util:decode(Body) of
        {ok, Decoded} ->
            receive_body(Decoded, SessionId),
            Success();
        {error, _} ->
            Req:respond(500, [], "Broken JSON encoding.")
    end.

receive_body(Decoded, SessionId) ->
    Sender = sockjs_session:sender(SessionId),
    [Sender ! {browser,Msg} || Msg <- Decoded].

% ---------------------------------------------------------------------------

headers(Req, Headers) ->
    headers(Req, Headers, "application/javascript; charset=UTF-8").

headers(Req, Headers, ContentType) ->
    Req:chunk(head,[{"Content-Type", ContentType}] ++ Headers).


chunk(Req, Body)      -> Req:chunk(Body).
chunk(Req, Body, Fmt) -> Req:chunk(Fmt(Body)).

fmt_xhr(Body) -> <<Body/binary, $\n>>.

fmt_jsonp(Body, Callback) ->
    %% Yes, JSONed twice, there isn't a a better way, we must pass
    %% a string back, and the script, will be evaled() by the
    %% browser.
    Double = sockjs_util:encode(Body),
    <<Callback/binary, "(", Double/binary, ");", $\r, $\n>>.

fmt_eventsource(Body) ->
    Escaped = iolist_to_binary(
                url_escape(binary_to_list(Body),
                           [$%, $\r, $\n, 0])), %% $% must be first!
    <<"data: ", Escaped/binary, $\r, $\n, $\r, $\n>>.

fmt_htmlfile(Body) ->
    Double = sockjs_util:encode(Body),
    <<"<script>", $\n, "p(", Double/binary, ");", $\n, "</script>", $\r, $\n>>.

fmt(Fmt, Args) -> iolist_to_binary(io_lib:format(Fmt, Args)).

url_escape(Str, Chars) ->
    [case lists:member(Char, Chars) of
         true  -> hex(Char);
         false -> Char
     end || Char <- Str].

hex(C) ->
    <<High0:4, Low0:4>> = <<C>>,
    High = integer_to_list(High0),
    Low = integer_to_list(Low0),
    "%" ++ High ++ Low.

-define(WS_MODULE, sockjs_ws).

ws_loop(Ws, Loop) ->
    process_flag(trap_exit,true),
    Ws:send(["o"]),
    Self = {?WS_MODULE, Ws},
    Ws_loop = spawn_link(fun() -> Loop(Self) end),
    ws_loop0(Self,Ws_loop).

ws_loop0(Self,Ws_loop) ->
    receive
        {browser, ""} ->
            ws_loop0(Self, Ws_loop);
        {browser, Data} ->
            case sockjs_util:decode(Data) of
                {ok, Decoded} ->
                    Ws_loop ! {browser, Decoded},
                    ws_loop0(Self, Ws_loop);
                {error, _} ->
		    Self:close(500,"Broken JSON encoding.'"),
                    closed
            end;
        closed ->
            Ws_loop ! closed;
	{'EXIT',_,_,_} ->
	    closed;
        Msg ->
	    Ws_loop ! Msg,
            ws_loop0(Self, Ws_loop)
    end.

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
