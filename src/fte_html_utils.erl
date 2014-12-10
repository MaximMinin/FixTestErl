%% @author mininm
%% @doc @todo Add description to fte_mainwindow.


-module(fte_html_utils).
-include_lib("yaws/include/yaws_api.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([available_testcases/0, register_for_events/1, upload/1]).

-record(upload, {
          fd,
          filename,
          last}).

upload(A) when A#arg.state == undefined ->
    State = #upload{},
    multipart(A, State);
upload(A) ->
    multipart(A, A#arg.state).

err() ->
    {done, {redirect, "/upload.yaws?upload_not_successful"}}.

multipart(A, State) ->
    Parse = yaws_api:parse_multipart_post(A),
    case Parse of
        {cont, Cont, Res} ->
            case addFileChunk(A, Res, State) of
                {done, Result} ->
                    Result;
                {cont, NewState} ->
                    {get_more, Cont, NewState}
            end;
        {result, Res} ->
            case addFileChunk(A, Res, State#upload{last=true}) of
                {done, Result} ->
                    Result;
                {cont, _} ->
                    err()
            end;
        {error, _Reason} ->
            err()
    end.



addFileChunk(A, [{part_body, Data}|Res], State) ->
    addFileChunk(A, [{body, Data}|Res], State);

addFileChunk(_A, [], State) when State#upload.last==true,
                                 State#upload.filename /= undefined,
                                 State#upload.fd /= undefined ->

    file:close(State#upload.fd),
    Name = State#upload.filename,
    Module = erlang:list_to_atom(string:sub_word(Name, 1, $.)),
    {ok, DIR} =  application:get_env(fixTestErl, test_cases_dir),
    case compile:file(DIR ++ Name, [{outdir,DIR},verbose,return_errors]) of
        {ok, Module} ->
            lager:info("Compiled: ~p", [Module]),
            code:purge(Module),
            code:load_file(Module),
            {done, {redirect, "/upload.yaws?upload_successful"}};
        Error ->
            lager:info("/upload.yaws?upload_not_successful ~p", [Error]),
            {done, {redirect, io_lib:format("/upload.yaws?upload_not_successful ~p", 
                                                       [Error])}}
    end;

addFileChunk(A, [], State) when State#upload.last==true ->
    {done, err()};

addFileChunk(_A, [], State) ->
    {cont, State};

addFileChunk(A, [{head, {_Name, Opts}}|Res], State ) ->
    case lists:keysearch("filename", 1, Opts) of
        {value, {_, Fname0}} ->
            Fname = yaws_api:sanitize_file_name(basename(Fname0)),

            %% we must not put the file in the
            %% docroot, it may execute uploade code if the
            %% file is a .yaws file !!!!!
            {ok, DIR} =  application:get_env(fixTestErl, test_cases_dir),
  	    case file:open([DIR, Fname] ,[write]) of
		{ok, Fd} ->
		    S2 = State#upload{filename = Fname,
				      fd = Fd},
		    addFileChunk(A, Res, S2);
		Err ->
		    {done, err()}
	    end;
	false ->
            addFileChunk(A,Res,State)
    end;

addFileChunk(A, [{body, Data}|Res], State)
  when State#upload.filename /= undefined ->
    case file:write(State#upload.fd, Data) of
        ok ->
            addFileChunk(A, Res, State);
        Err ->
            {done, err()}
    end.


basename(FilePath) ->
    case string:rchr(FilePath, $\\) of
        0 ->
            %% probably not a DOS name
            filename:basename(FilePath);
        N ->
            %% probably a DOS name, remove everything after last \
            basename(string:substr(FilePath, N+1))
    end.

register_for_events(A)->
    H=A#arg.headers,
    C = H#headers.cookie,
    L=yaws_api:find_cookie_val("foobar", C),
    EventPublisher = erlang:list_to_atom(L),
    [T|[]] = lists:sublist(binary:split(A#arg.clidata, <<"=">>), 2, 1),
    TestCase = erlang:binary_to_atom(T, utf8),
    event_publisher:register_test_case(EventPublisher, TestCase).

available_testcases() ->
B = code:all_loaded(),
AvailableTests =  lists:filtermap(fun({_,Name}) -> 
                    case Name of
                        preloaded ->
                            false;
                        _ ->  
                             C = string:words(Name, $/),
                             M = string:sub_word(Name, C, $/),
                             Module = string:sub_word(M, 1, $.),
                             T = erlang:list_to_existing_atom(Module),
                             case T of
								 '' -> false;
								 _ ->
									 Att = lists:keyfind([test_case_template], 2, T:module_info(attributes)),
									 case Att of
										 false -> false;
										 _ -> {true, Module}
									 end
							 end
					end
			   end, B),
Testcases = lists:filter(fun(X) -> case X of
                                    {registered_name, _} -> true; 
                                    _else -> false 
                                    end 
                         end, lists:flatmap(fun(X) ->
                                                    erlang:process_info(X) end,
                                            lists:map(fun({_,Pid,_,_}) -> Pid end, 
                                                      supervisor:which_children(fixTestErl_sup)
                                                      )
                                            )
                          ),
RunningTests = lists:map(fun({_, Name}) -> 
                                 N = erlang:atom_to_list(Name),
                                 lists:sublist(N, 5, erlang:length(N)-4) 
                         end, Testcases),
Header = {tr, [], [
               {th, [], {p, [{class, "foo"}], "Name"}},
               {th, [], {p, [{class, "foo"}], "Mode"}},
               {th, [], {p, [{class, "foo"}], "Port"}},
               {th, [], {p, [{class, "foo"}], "Fix version"}},
               {th, [], {p, [{class, "foo"}], "Server"}},
               {th, [], {p, [{class, "foo"}], "Start / Stop"}},
               {th, [], {p, [{class, "foo"}], "Showlog *"}}
              ]},
Rows = lists:map(fun(T) -> 
           Start = case lists:member(T, RunningTests) of
                false ->
                   {td, [{align, "center"}],
                  {form, [{method, "post"}, {action, "/start/"++T}], 
                  {input, [{type, "submit"}, {value, "start"}]}}
                   };
                true ->
                   {td, [{align, "center"}],
                  {form, [{method, "post"}, {action, "/stop/"++T}], 
                  {input, [{type, "submit"}, {value, "stop"}]}}
                   }
           end,
           Log = case lists:member(T, RunningTests) of
                true ->
                   {td, [{align, "center"}],
                  {form, [{method, "post"}, {action, "/showLog/"++T}], 
                  {input, [{type, "submit"}, {value, "showLog"}]}}
                   };
                false ->
                   {td, [], {p, [{class, "foo"}], ""}}
           end,
           M = erlang:list_to_atom(T),
           {ok, DIR} =  application:get_env(fixTestErl, test_cases_dir),
           case file:read_file(DIR ++ T ++ ".erl") of
                {ok, F} -> 
                   {tr, [],
                       [
                         {td, [{title, erlang:binary_to_list(binary:replace(F, <<"\"">>, <<"|">>, [global]))}], {p, [{class, "foo"}], T}},
        %%                {td, [], {p, [{class, "foo"}], T}},
                          {td, [], {p, [{class, "foo"}], lists:concat(["", M:get_mod()])}}, 
                         {td, [], {p, [{class, "foo"}], lists:concat(["", M:get_port()])}}, 
                         {td, [], {p, [{class, "foo"}], erlang:atom_to_list(M:get_fix_version())}},
                         {td, [], {p, [{class, "foo"}], lists:concat(["", M:get_ip()])}},
                         Start,
                         Log
                        ]
                   };
                _Else -> {tr, [],[]}
            end
  end, 
AvailableTests),

TabT= {table, [{class, "table table-bordered"}], [Header|Rows]},
{ehtml, TabT}.


%% ====================================================================
%% Internal functions
%% ====================================================================


