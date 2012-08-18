%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(fixTestErl_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/0, start_child/1, auto_start/0]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_child(Callback) ->
    supervisor:start_child(?MODULE, [Callback, Callback:get_mod(), Callback:get_ip(), Callback:get_port(), Callback:get_fix_version()]).
auto_start() ->
   case application:get_env(fixTestErl, callbacks) of
       {ok, Callbacks} ->
           lists:map(fun(Callback) -> fixTestErl_sup:start_child(Callback) end, Callbacks);
       _Else ->
           ok
    end.


%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([]) ->
    {ok,{{simple_one_for_one,10,10}, [
                                { 
                                    test_case_root_sup, 
                                    {test_case_root_sup, start_link, []},
                                    permanent, infinity, supervisor,
                                    [test_case_root_sup]
                                }
                              ]}}.
