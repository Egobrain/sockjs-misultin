-module(sockjs_ws).
-behaviour(sockjs_sender).

-export([send/2, close/3]).

send(Data, {?MODULE, Ws}) ->
    Ws:send(["a[", sockjs_util:encode(Data), "]"]).

close(Code, Reason, {?MODULE, Ws}) ->
    Ws:send(["c", sockjs_util:encode([Code, list_to_binary(Reason)])]),
    exit(normal). %% TODO ?

