%% Author: Maxim Minin
%% Created: 28.05.2012
%% Description: TODO: Add description to test_case_template
-module(test_case_template).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([behaviour_info/1]).

%%
%% API Functions
%%
behaviour_info(callbacks) ->
    [
     {reply, 3},
     {ini, 0}
    ];
behaviour_info(_Other) ->
    undefined.


%%
%% Local Functions
%%

