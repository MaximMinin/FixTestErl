%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(fixTestErl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    fixTestErl_sup:start_link(quote, server, localhost, 12345, "FIX_4_2"),
    fixTestErl_sup:start_link(quoteRequest, client, localhost, 12345, "FIX_4_2").

stop(_State) ->
    ok.
