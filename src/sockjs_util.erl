-module(sockjs_util).

-export([encode/1, encode/2, decode/1, encode_list/1]).

encode_list([{close, {Code, Reason}}]) ->
    encode(<<"c">>, [Code, list_to_binary(Reason)]);
encode_list([{open, _}]) ->
    <<"o">>;
encode_list(L) ->
    encode(<<"a">>, [D || {data, D} <- L]).

encode(Prefix, Thing) ->
    JSON = encode(Thing),
    <<Prefix/binary, JSON/binary>>.

encode(Thing) ->
    iolist_to_binary(jiffy:encode(Thing)).

decode(Thing) ->
    JSON = iolist_to_binary(Thing),
    try jiffy:decode(JSON) of
        V -> {ok, V}
    catch
        E -> E
    end.

