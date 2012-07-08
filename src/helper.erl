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
%% <<"20000426-12:05:06">> 
getNow(AddTimeInSec) ->
    AddSec = AddTimeInSec rem 60,
    AddMin = AddTimeInSec div 60,
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(Year),
                                        getTwoDigits(Month),
                                        getTwoDigits(Day),
                                        "-",
                                        getTwoDigits(Hour),
                                        ":",
                                        getTwoDigits(Minute+AddMin),
                                        ":",
                                        getTwoDigits(Second+AddSec)
                                       ]
                                      )
                         ).
getNow() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    erlang:list_to_binary(lists:concat([
                                        erlang:integer_to_list(Year),
                                        getTwoDigits(Month),
                                        getTwoDigits(Day),
                                        "-",
                                        getTwoDigits(Hour),
                                        ":",
                                        getTwoDigits(Minute),
                                        ":",
                                        getTwoDigits(Second)
                                       ]
                                      )
                         ).
getTwoDigits(Int) when Int < 10 ->
    lists:concat(["0",Int]);
getTwoDigits(Int) ->
    erlang:integer_to_list(Int).

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

