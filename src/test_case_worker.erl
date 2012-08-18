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
         getMessages/2, getMessages/3, 
         send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(worker, {name, count_in = 0, count_out = 0}).
-record(state, {callback, archive_in, archive_out, mode}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Name, Mode) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name, Mode], []).

newMessage(Name, Message)->
    gen_server:cast(Name, {message, Message}).
getMessages(Name, RegExp) ->
    gen_server:call(Name, {getMessages, RegExp}).
getMessages(Name, From, To) ->
    gen_server:call(Name, {getMessages, From, To}).
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
    InT = ets:new(erlang:list_to_atom(lists:concat([in_, Name])), [ordered_set ]),
    OutT = ets:new(erlang:list_to_atom(lists:concat([out_, Name])), [ordered_set ]),
    case ets:info(stateOfCallbacks) of
        undefined ->
            ets:new(stateOfCallbacks, [ordered_set, public, named_table]);
        _Else ->
            ok
    end,
    case Mode of
        client ->
            gen_server:cast(Name, {ini});
        server ->
            ok
    end,
    State = #state{callback = #worker{name = Name}, archive_in = InT, archive_out = OutT, mode = Mode},
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
handle_call({getMessages, RegExp}, _From, #state{archive_out = ETS} = State) ->
    ToReturn = ets:select(ETS, RegExp),
    {reply, ToReturn, State};
handle_call({getMessages, From, To}, _From, #state{archive_out = ETS} = State) ->
    ToReturn = ets:select(ETS, [{'$1', 
                                 [{'and',
                                        {'=<', From, {element, 1, '$1'}}, 
                                        {'>=', To, {element, 1, '$1'}}
                                   }],
                                 [{element, 3, '$1'}]}]),
    {reply, ToReturn, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({send, Messages}, #state{callback = W, archive_out = A} = State) ->
    #worker{count_out = Count, name = C} = W,
    NewCount = reply(Messages, Count, C, A),
    NewState = State#state{callback = W#worker{count_out = NewCount}},
    ets:insert(stateOfCallbacks, {C, W#worker{count_out = NewCount}}),
    {noreply, NewState};
handle_cast({ini}, #state{callback = W, archive_out = A} = State) ->
    #worker{count_out = Count, name = C} = W,
    Messages = C:ini(),
    NewCount = reply(Messages, Count, C, A),
    NewState = State#state{callback = W#worker{count_out = NewCount}},
    ets:insert(stateOfCallbacks, {C, W#worker{count_out = NewCount}}),
    {noreply, NewState};
handle_cast({message, Msg}, #state{callback = W, archive_in = A,  archive_out = Ar} = State) ->
    #worker{count_in = I, count_out = Count, name = C} = W,
    ets:insert(A, {I+1, erlang:now(), Msg}),
    NewCount = case C:reply(Msg, I, Count) of
        ok ->
            Count;
        Messages ->
            reply(Messages, Count, C, Ar)
    end,
    NewState = State#state{callback = W#worker{count_in = I+1, count_out = NewCount}},
    ets:insert(stateOfCallbacks, {C, W#worker{count_in = I+1, count_out = NewCount}}),
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
    io:format("REASON: ~p~n", [Reason]),
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
reply([M|Messages], Count, Callback, ETS) ->
    split_server:newReplyData(M, Callback), 
    ets:insert(ETS, {Count+1, erlang:now(), M}),
    reply(Messages, Count+1, Callback, ETS);
reply([], Count, _Callback, _ETS)->
    Count.

getLastNelements(Tab, N) ->
    getLastNelements(Tab, N, ets:last(Tab), []).

getLastNelements(_Tab, _N, '$end_of_table', ToReturn)->
   ToReturn;
getLastNelements(_Tab, 0, _Key, ToReturn)->
    ToReturn;
getLastNelements(Tab, N, Key, ToReturn)->
    [E] = ets:lookup(Tab, Key),
    case ets:prev(Tab, Key) of
        '$end_of_table' ->
	   [E|ToReturn];
	Key1 ->
	    [E2] = ets:lookup(Tab, Key1),
	    getLastNelements(Tab, N-1, Key1, [E2|[E|ToReturn]])
    end.
