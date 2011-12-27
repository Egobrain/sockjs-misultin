-module(sockjs_session_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

start_link() ->
     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 10}, []}}.

start_child(SessionId, Loop, Opts) ->
    supervisor:start_child(
      ?MODULE,
      {SessionId, {sockjs_session, start_link, [SessionId, Loop, Opts]},
       transient, 16#ffffffff, worker, [sockjs_session]}).
