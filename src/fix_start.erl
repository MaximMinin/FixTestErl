-module(fix_start).

-include_lib("yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

out(#arg{appmoddata = Testcase}) ->
fixTestErl_sup:start_child(erlang:list_to_atom(Testcase)),
lager:info("STARTEN: ~p", [Testcase]),
{redirect, "/scenarios.yaws"}.

