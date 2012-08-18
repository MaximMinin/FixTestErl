%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(split_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
-export([start_link/2, newRowData/2, newReplyData/2, register/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last = <<>>, clientPid, version, web_logs = []}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Testcase, FixVersion)->
    gen_server:start_link({local, erlang:list_to_atom(lists:concat([splitt_, Testcase]))}, ?MODULE, [Testcase, FixVersion], []).

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
init([Client, FixVersion]) ->
    {ok, #state{clientPid = Client, version = FixVersion}}.

newRowData(Data, Testcase) ->
    gen_server:cast(erlang:list_to_atom(lists:concat([splitt_, Testcase])), {new, Data}).
newReplyData(Rec, Testcase) ->
    gen_server:cast(erlang:list_to_atom(lists:concat([splitt_, Testcase])), {reply, Rec}).
register(Testcase) ->
    gen_server:cast(erlang:list_to_atom(lists:concat([splitt_, Testcase])), {register, self()}).
    

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
handle_cast({register, Pid}, #state{web_logs=W}=State) ->
    {noreply, State#state{web_logs=[Pid|W]}};
handle_cast({reply, Rec}, #state{version = V, clientPid = C, web_logs = W} = State) ->
    lists:map(fun(Pid) -> Pid !{out, convertor:format(Rec, V)} end, W),
    tcp_server:send(C, convertor:convertRecordtoFix(Rec, V)),
    {noreply, State};
handle_cast({new, Data}, #state{last = Last, clientPid = ClientPid, version = V, web_logs = W} = State) ->
    {Broken, Messages} = split(binary:list_to_bin([Last, Data])),
    lists:map(fun(M) ->
              Reg = convertor:convertFixToRecord(M, V), 
              test_case_worker:newMessage(ClientPid, Reg),
              lists:map(fun(Pid) -> Pid ! {in, convertor:format(Reg, V)} end, W)
              end, Messages),
    {noreply, State#state{last = Broken}}.

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
split(Bin) ->
    [L|Liste] = binary:split(Bin, <<"10=">>, [global]),
    split(Liste, L, []).

split([E|[]], Last, ToReturn) ->
    case binary:split(E, <<1>>) of 
        [<<>>] ->
            {Last, ToReturn};
        [Int|Rest] ->
             {binary:list_to_bin(Rest), [binary:list_to_bin([Last, <<"10=">>, Int, <<1>>])|ToReturn]}
    end;
split([E|Liste], Last, ToReturn) ->
    [Int|Rest] =  binary:split(E, <<1>>), 
    split(Liste, Rest, [binary:list_to_bin([Last, <<"10=">>, Int, <<1>>])|ToReturn]). 
