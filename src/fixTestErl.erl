%% Author: Maxim Minin
%% Created: 03.06.2012
%% Description: TODO: Add description to fixTestErl
-module(fixTestErl).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _Args) ->
    {ok, DIR} =  application:get_env(fixTestErl, test_cases_dir),
    file:make_dir(DIR),
    {ok, Files} = file:list_dir(DIR),
    code:add_path(DIR),
    lists:map(fun(File) -> 
              case string:substr(File, string:len(File)-4) of
                  ".beam" ->
		      code:load_abs(DIR ++ string:substr(File, 1, string:len(File)-5));
                  _Else ->
                      ok
              end end, Files),
    code:rehash(),
    fixTestErl_rootsup:start_link().

stop(_State) ->
    ok.
