-module(fix_showlog).

-include("../deps/yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

out(A) ->
    T = A#arg.appmoddata,
    H=A#arg.headers,
    C = H#headers.cookie,
    L=yaws_api:find_cookie_val("foobar", C),
    EventPublisher = erlang:list_to_atom(L),
    TestCase = erlang:list_to_atom(T),
    event_publisher:register_test_case(EventPublisher, TestCase),
{redirect, "/mainwindow.yaws"}.