-module(fix_stop).

-include_lib("yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

out(#arg{appmoddata = Testcase}) ->
fixTestErl_sup:stop_child(erlang:list_to_atom(Testcase)),
lager:info("STOP: ~p", [Testcase]),
{redirect, "/scenarios.yaws"}.

