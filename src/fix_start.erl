-module(fix_start).

-include("../deps/yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

out(#arg{appmoddata = Testcase}) ->
fixTestErl_sup:start_child(erlang:list_to_atom(Testcase)),
io:format("STARTEN: ~p~n", [Testcase]),
{redirect, "/mainwindow.yaws"}.

