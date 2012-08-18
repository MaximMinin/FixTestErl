%%% -------------------------------------------------------------------
%%% Author  : Maxim Minin
%%% Description : TODO
%%%
%%% Created : 18.08.2012
%%% -------------------------------------------------------------------
-module(test_case_root_sup).

-behaviour(supervisor).
%% --------------------------------------------------------------------
%% External exports
%% --------------------------------------------------------------------
-export([start_link/5]).

%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
start_link(Testcase, Mode, Ip, Port, FixVersion) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Testcase, Mode, Ip, Port, FixVersion]).



%% ====================================================================
%% Server functions
%% ====================================================================
%% --------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%% --------------------------------------------------------------------
init([Testcase, Mode, Ip, Port, FixVersion]) ->
    {ok,{{one_for_one,10,10}, [
                                {
                                    test_archive_sup,
                                    {test_archive_sup, start_link, []},
                                    permanent, infinity, supervisor,
                                    [test_archive_sup]
                                },
                                { 
                                    test_case_sup, 
                                    {test_case_sup, start_link, [Testcase, Mode, Ip, Port, FixVersion]},
                                    permanent, infinity, supervisor,
                                    [test_case_sup]
                                }
                              ]}}.

