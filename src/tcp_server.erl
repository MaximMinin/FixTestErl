%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(tcp_server).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/4, send/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {socket, test_case_name, mode}).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Testcase, Mode, Ip, Port) ->
    gen_server:start_link({local, erlang:list_to_atom(lists:concat([tcp_, Testcase]))}, ?MODULE, [Testcase, Mode, Ip, Port], []).

%% ====================================================================
%% Server functions
%% ====================================================================
send(Testcase, Bin) ->
    gen_server:cast(erlang:list_to_atom(lists:concat([tcp_, Testcase])), {send, Bin}).
%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Testcase, Mode, Ip, Port]) ->
    case Mode of
        server ->
            {ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}, {reuseaddr, true}]);
        client ->
            {ok, Socket} = gen_tcp:connect(Ip, Port, [binary, {active, true}, {reuseaddr, true}])
    end,
    gen_server:cast(self(), {init, Socket}),
    {ok, #state{test_case_name = Testcase, mode = Mode}}.

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
handle_cast({send, Bin}, #state{socket = Socket } = State) ->
    gen_tcp:send(Socket, Bin),
    {noreply, State};
handle_cast({init, S}, #state{mode = Mode, test_case_name = T } = State) ->
    case Mode of
        server ->
            {ok, Socket} = gen_tcp:accept(S),
            gen_server:cast(T, {ini});
        client ->
            Socket = S
    end,
    {noreply, State#state{socket = Socket}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({tcp, _Port, Bin}, #state{test_case_name = T} = State) ->
    split_server:newRowData(Bin, T),
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    {stop, tcp_closed, State};
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

