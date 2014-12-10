%%% -------------------------------------------------------------------
%%% Author  : mir
%%% Description :
%%%
%%% Created : 27.05.2012
%%% -------------------------------------------------------------------
-module(test_case_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, newMessage/2, 
         send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-record(worker, {name, count_in = 0, count_out = 0}).
-record(state, {callback, mode}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Name, Mode) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Mode], []).

newMessage(Name, Message)->
    gen_server:cast(Name, {message, Message}).

send(Name, Messages)->
    gen_server:cast(Name, {send, Messages}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Name, Mode]) ->
    case ets:info(stateOfCallbacks) of
        undefined ->
            ets:new(stateOfCallbacks, [ordered_set, 
                                       public, named_table]);
        _Else ->
            ok
    end,
    case Mode of
        client ->
            gen_server:cast(Name, {ini});
        server ->
            ok
    end,
    State = #state{callback = #worker{name = Name}, mode = Mode},
    {ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({send, Messages}, #state{callback = W} = State) ->
    #worker{count_out = Count, name = C} = W,
    NewCount = reply(Messages, Count, C),
    NewState = State#state{callback = W#worker{count_out = NewCount}},
    ets:insert(stateOfCallbacks, {C, W#worker{count_out = NewCount}}),
    {noreply, NewState};
handle_cast({ini}, #state{callback = W} = State) ->
    #worker{count_out = Count, name = C} = W,
    Messages = C:ini(),
    NewCount = reply(Messages, Count, C),
    NewState = State#state{callback = W#worker{count_out = NewCount}},
    ets:insert(stateOfCallbacks, {C, W#worker{count_out = NewCount}}),
    {noreply, NewState};
handle_cast({message, Msg}, #state{callback = W} = State) ->
    #worker{count_in = I, count_out = Count, name = C} = W,
    test_archive_worker:insert(C, {in, {I+1, erlang:now(), Msg}}),
    NewCount = case C:reply(Msg, I, Count) of
        ok ->
            Count;
        Messages ->
            reply(Messages, Count, C)
    end,
    NewState = State#state{callback = 
                    W#worker{count_in = I+1, count_out = NewCount}},
    ets:insert(stateOfCallbacks, 
                {C, W#worker{count_in = I+1, count_out = NewCount}}),
    {noreply, NewState}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(Reason, _State) ->
    lager:info("REASON: ~p", [Reason]),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------
reply([M|Messages], Count, Callback) ->
    split_server:newReplyData(M, Callback), 
    test_archive_worker:insert(Callback, 
                               {out, {Count+1, erlang:now(), M}}),
    reply(Messages, Count+1, Callback);
reply([], Count, _Callback)->
    Count.

