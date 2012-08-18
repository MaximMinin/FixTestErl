%%% -------------------------------------------------------------------
%%% Author  : mir
%%% Description :
%%%
%%% Created : 18.08.2012
%%% -------------------------------------------------------------------
-module(test_archive_worker).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/1, insert/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {in ,out}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Testcase)->
    gen_server:start_link({local, erlang:list_to_atom(lists:concat([archiv_, Testcase]))}, ?MODULE, [Testcase], []).

insert(Testcase, Rec) ->
    gen_server:cast(erlang:list_to_atom(lists:concat([archiv_, Testcase])), Rec).


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
init([Name]) ->
    InT = ets:new(erlang:list_to_atom(lists:concat([in_, Name])), [ordered_set ]),
    OutT = ets:new(erlang:list_to_atom(lists:concat([out_, Name])), [ordered_set ]),
    case ets:info(stateOfCallbacks) of
        undefined ->
            ets:new(stateOfCallbacks, [ordered_set, public, named_table]);
        _Else ->
            ok
    end,
    {ok, #state{in = InT, out = OutT}}.

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
handle_call({getMessages, RegExp}, _From, #state{out = ETS} = State) ->
    ToReturn = ets:select(ETS, RegExp),
    {reply, ToReturn, State};
handle_call({getMessages, From, To}, _From, #state{out = ETS} = State) ->
    ToReturn = ets:select(ETS, [{'$1', 
                                 [{'and',
                                        {'=<', From, {element, 1, '$1'}}, 
                                        {'>=', To, {element, 1, '$1'}}
                                   }],
                                 [{element, 3, '$1'}]}]),
    {reply, ToReturn, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({in, Rec}, #state{in = ETS} = State) ->
    ets:insert(ETS, Rec),
    {noreply, State};
handle_cast({out, Rec}, #state{out = ETS} = State) ->
    ets:insert(ETS, Rec),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

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
terminate(_Reason, _State) ->
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

%% getLastNelements(Tab, N) ->
%%     getLastNelements(Tab, N, ets:last(Tab), []).
%% 
%% getLastNelements(_Tab, _N, '$end_of_table', ToReturn)->
%%    ToReturn;
%% getLastNelements(_Tab, 0, _Key, ToReturn)->
%%     ToReturn;
%% getLastNelements(Tab, N, Key, ToReturn)->
%%     [E] = ets:lookup(Tab, Key),
%%     case ets:prev(Tab, Key) of
%%         '$end_of_table' ->
%%        [E|ToReturn];
%%     Key1 ->
%%         [E2] = ets:lookup(Tab, Key1),
%%         getLastNelements(Tab, N-1, Key1, [E2|[E|ToReturn]])
%%     end.
