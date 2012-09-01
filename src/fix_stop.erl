-module(fix_stop).

-include("../deps/yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

out(#arg{appmoddata = Testcase}) ->
fixTestErl_sup:stop_child(erlang:list_to_atom(Testcase)),
io:format("STOP: ~p~n", [Testcase]),
{redirect, "/mainwindow.yaws"}.

