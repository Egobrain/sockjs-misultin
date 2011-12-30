-module(sockjs_session).

-behaviour(sockjs_sender).
-behaviour(gen_server).

-export([start_link/2, maybe_create/2, sender/1, reply/3]).

-export([send/2, close/3, session/1]).

-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3,
         handle_cast/2]).

-record(session, {id,
		  outbound_queue,
		  response_pid,
		  receiver,
                  session_timeout,
		  closed = false,
		  close_msg,
		  ws_loop
		 }).

start_link(SessionId, Receive) ->
    gen_server:start_link(?MODULE, {SessionId, Receive}, []).

maybe_create(dummy, _) ->
    ok;

maybe_create(SessionId, Loop) ->
    % TODO fix bugs
    case sockjs_session_sup:start_child(SessionId, Loop) of
	{error,{already_started,SPid}} ->
	    SPid;
	{error,already_present} ->
	    gproc:lookup_local_name(SessionId);
	{ok,SPid} ->
	    SPid
    end.

send(Data, {?MODULE, SessionId}) ->
    enqueue({data, Data}, SessionId).

close(Code, Reason, {?MODULE, SessionId}) ->
    enqueue({close, {Code, Reason}}, SessionId),
    exit(normal).
session({?MODULE, SessionId}) ->
    gen_server:call(spid(SessionId), session).

enqueue(Cmd, SessionId) ->
    gen_server:cast(spid(SessionId), {enqueue, Cmd}).

sender(SessionId) ->
    gen_server:call(spid(SessionId), ws_loop).

reply(SessionId, Once, Req) ->
    gen_server:call(spid(SessionId), {reply, self(), Once, Req}, infinity).

%% --------------------------------------------------------------------------

pop_from_queue(Q) ->
    {PoppedRev, Rest} = pop_from_queue(any, [], Q),
    {lists:reverse(PoppedRev), Rest}.

pop_from_queue(TypeAcc, Acc, Q) ->
    case queue:peek(Q) of
        {value, {Type, _}} when TypeAcc =:= any orelse TypeAcc =:= Type ->
            {{value, Val}, Q2} = queue:out(Q),
            pop_from_queue(Type, [Val | Acc], Q2);
        _ -> {Acc, Q}
    end.

spid(SessionId) ->
    case gproc:lookup_local_name(SessionId) of
        undefined ->
	    throw(no_session);
        SPid -> SPid
    end.

maybe_close([{close, _}] = Msg, State = #session{closed = false}) ->
    State#session{closed = true, close_msg = Msg};
maybe_close([{close, _}],       _State) ->
    exit(assertion_failed);
maybe_close(_,                  State) ->
    State.

reply3(Reply, Pid, State = #session{response_pid    = undefined, session_timeout = Ref})  ->
    link(Pid),
    case Ref of
        undefined -> ok;
        _         -> erlang:cancel_timer(Ref)
    end,
    reply3(Reply, Pid, State#session{response_pid    = Pid,
                                    session_timeout = undefined});
reply3(Reply, Pid, State = #session{response_pid = Pid}) ->
    {reply, Reply, State}.

%% --------------------------------------------------------------------------
init({SessionId, Loop}) ->
    gproc:add_local_name(SessionId),
    process_flag(trap_exit, true),
    WS_LOOP = spawn_link(fun() ->
				 try
				     Loop({?MODULE, SessionId})
				 catch
				     throw:no_session ->
					 exit(no_session)
				 end
			 end),
    {ok, #session{id = SessionId, receiver = Loop, ws_loop = WS_LOOP, outbound_queue=queue:in({open,nil},queue:new())}}.

%% For non-streaming transports we want to send a closed message every time
%% we are asked - for streaming transports we only want to send it once.
handle_call({reply, Pid, true,_Req}, _From, State = #session{closed    = true, close_msg = Msg}) ->
    reply3(sockjs_util:encode_list(Msg), Pid, State);
handle_call({reply, Pid, _Once, Req}, From, State = #session{response_pid   = RPid, outbound_queue = Q}) ->
    case {pop_from_queue(Q), RPid} of
        {{[], _}, P} when P =:= undefined orelse P =:= Pid ->
            reply3(wait, Pid, State);
        {{[], _}, _} ->
            %% don't use reply(), this shouldn't touch the session lifetime
            {reply, session_in_use, State};
        {{Popped, Rest}, _} ->
	    case Popped of
		[{session,To}] ->
		    gen_server:reply(To,Req:session()),
		    handle_call({reply,Pid,_Once,Req},From, State#session{outbound_queue = Rest});
		_ ->
		    State1 = maybe_close(Popped, State),
		    reply3(sockjs_util:encode_list(Popped), Pid,State1#session{outbound_queue = Rest})
	    end
    end;
handle_call(ws_loop,_From, State) ->
    {reply,State#session.ws_loop,State};
handle_call(session, From, State = #session{outbound_queue = Q}) ->
    {noreply, State#session{outbound_queue = queue:in({session,From},Q)}};

handle_call(Request, _From, State) ->
    {stop, {odd_request, Request}, State}.



handle_cast({enqueue, Cmd}, State = #session{outbound_queue = Q, response_pid   = P}) ->
    if is_pid(P) -> P ! go;
       true      -> ok
    end,
    {noreply, State#session{outbound_queue = queue:in(Cmd, Q)}};

handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.

handle_info({'EXIT', Pid, _Reason}, State = #session{response_pid = Pid}) ->
    {ok, CloseTime} = application:get_env(sockjs, session_close_ms),
    Ref = erlang:send_after(CloseTime, self(), session_timeout),
    {noreply, State#session{response_pid    = undefined, session_timeout = Ref}};

handle_info(session_timeout, State = #session{response_pid = undefined}) ->
    {stop, normal, State};

handle_info({'EXIT',Pid, _Reason}, State = #session{ws_loop = Pid}) ->
    {ok, CloseTime} = application:get_env(sockjs, session_close_ms),
    Ref = erlang:send_after(CloseTime, self(), session_timeout),
    {noreply, State#session{response_pid    = undefined, session_timeout = Ref}};

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.

terminate(_Reason, #session{ws_loop = Ws_loop}) ->
    case Ws_loop of
	Pid when is_pid(Pid) ->
	    Pid ! closed;
	undefined ->
	    closed
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

