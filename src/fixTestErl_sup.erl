%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(fixTestErl_sup).

-behaviour(supervisor).

%% API
-export([start_link/5]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Arg), {I, {I, start_link, Arg}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Testcase, Mode, Ip, Port, FixVersion) ->
    supervisor:start_link(?MODULE, [Testcase, Mode, Ip, Port, FixVersion]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Testcase, Mode, Ip, Port, FixVersion]) ->
    {ok, { {one_for_one, 5, 10}, [
                                  ?CHILD(tcp_server, worker, [Testcase, Mode, Ip, Port]),
                                  ?CHILD(split_server, worker, [Testcase, FixVersion]),
                                  ?CHILD(test_case_worker, worker, [Testcase, Mode])
                                  ]} }.

