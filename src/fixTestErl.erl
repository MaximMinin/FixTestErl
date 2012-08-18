%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(fixTestErl).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _Args) ->
    io:format("START~n"),
    {ok, Callback} = application:get_env(fixTestErl, callback),
    ybed_sup:start_link(),
    fixTestErl_sup:start_link(Callback, Callback:get_mod(), Callback:get_ip(), Callback:get_port(), Callback:get_fix_version()).
%%     fixTestErl_sup:start_link(quote, server, localhost, 12345, "FIX_4_2"),
%%     fixTestErl_sup:start_link(quoteRequest, client, localhost, 12345, "FIX_4_2").

stop(_State) ->
    ok.
