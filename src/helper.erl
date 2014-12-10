%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to helper
-module(helper).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([getNow/0, getNow/1, getIniq/0]).

%%
%% API Functions
%%
getNow(AddTimeInSec) ->
    AddSec = AddTimeInSec rem 60,
    AddMin = AddTimeInSec div 60,
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
	{{Year, Month, Day}, {Hour, Minute+AddMin, Second+AddSec}}.

getNow() ->
    erlang:universaltime().


getIniq () ->
    {MegaSecs, Secs, MicroSecs} = erlang:now(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(MegaSecs),
                                        erlang:integer_to_list(Secs),
                                        erlang:integer_to_list(MicroSecs)
                                       ]
                                      )
                         ).



%%
%% Local Functions
%%

