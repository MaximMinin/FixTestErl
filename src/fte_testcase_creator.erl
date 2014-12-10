%% @author mininm
%% @doc @todo Add description to fte_testcase_creator.


-module(fte_testcase_creator).

%% ====================================================================
%% API functions
%% ====================================================================
-export([create_testcase_file/1]).
-compile([export_all]).

-record(reply, {in, out, fields=[]}).
   
create_testcase_file(L) -> 
    {_,L1}=lists:split(7, L),
    Reply = calculate_reply(L1),
	Name = case lists:keysearch("name", 1, L) of
			false -> false;
			{value, {"name", N}} -> N
		   end,
	Port = case lists:keysearch("port", 1, L) of
			false -> false;
			{value, {"port", N1}} -> N1
		   end,
	Mode = case lists:keysearch("mode", 1, L) of
			false -> false;
			{value, {"mode", N2}} -> N2
		   end,
	Host = case lists:keysearch("host", 1, L) of
			false -> false;
			{value, {"host", N3}} -> N3
		   end,
	FixVersion = case lists:keysearch("version", 1, L) of
			false -> false;
			{value, {"version", N4}} -> N4
		   end,
    H = case {Mode, Host} of
            {"server", ""} ->
                "localhost";
            _ -> Host
        end,
	{Name, get_file_header(Name, FixVersion, Mode, Port, H, Reply)}.
	

%% ====================================================================
%% Internal functions
%% ====================================================================
get_util_module("FIX 4.0") ->
    "FIX_4_0";
get_util_module("FIX 4.1") ->
    "FIX_4_1";
get_util_module("FIX 4.2") ->
    "FIX_4_2";
get_util_module("FIX 4.3") ->
    "FIX_4_3";
get_util_module("FIX 4.4") ->
    "FIX_4_4";
get_util_module("FIX 5.0") ->
    "FIX_5_0";
get_util_module("FIX 5.0 SP 1") ->
    "FIX_5_0SP1";
get_util_module("FIX 5.0 SP 2") ->
    "FIX_5_0SP2".


get_file_header(Name, FixVersion, Mode, Port, Host, Reply)->
	io_lib:format(
"%% Created: TODO
%% Description: TODO: Add description to TODO
-module(~s).

-behaviour(test_case_template).
%%
%% Include files
%%
-include_lib(\"fix_convertor/include/~s.hrl\").
%%
%% Exported Functions
%%
-export([reply/3, 
		 ini/0, 
		 get_mod/0, 
		 get_ip/0, 
		 get_port/0, 
		 get_fix_version/0]).
-compile([{parse_transform, lager_transform}]).

%%
%% API Functions
%%
get_mod() ->
    ~s.

get_port() ->
    ~s.

get_fix_version() ->
    '~s'.

get_ip() ->
    ~s.

~s
reply({R, NsF}, _SeqNumIn, _SeqNumOut) ->
    lager:info(\"NO REPLY FOR ~~p ~~p\", [R, NsF]),
	ok.

ini()->
    [].
",[Name, get_util_module(FixVersion), Mode, Port, FixVersion, Host, Reply]).


get_records_names(FixVersion) ->
    IncludeDir="/home/mininm/Programme/erlang_libs/FixTestErl/rel/fixTestErl/lib/fix_convertor-1.2.0/include/",%%TODO
    FileName = fte_testcase_creator:get_util_module(FixVersion)++".hrl",
    {ok, L} = epp:parse_file(IncludeDir++FileName,[IncludeDir],[]),
    N = lists:filtermap(fun(X) -> 
        case X of
            {attribute,_,record,{RecordName, _RecordFields}} -> 
                Name = erlang:atom_to_list(RecordName),
                case Name == "standardHeader" 
                        orelse Name == "standardTrailer"
                            orelse string:str(Name, "rgr_") == 1 of
                    true -> false;
                    false -> {true, Name}
                end;
        _E -> false end end, L),
    
    lists:sort(N).

get_records_fields(FixVersion, RecordName) ->
    [{fieldset, [{id, RecordName}, {name, RecordName}, {style, "display: none"}], 
      get_records_fields2(FixVersion, RecordName)}].

get_records_fields2(FixVersion, RecordName) when is_list(RecordName)->
    get_records_fields2(FixVersion, list_to_atom(RecordName));
get_records_fields2(FixVersion, RecordName) when is_atom(RecordName)->
    IncludeDir="/home/mininm/Programme/erlang_libs/FixTestErl/rel/fixTestErl/lib/fix_convertor-1.2.0/include/",%%TODO
    FileName = fte_testcase_creator:get_util_module(FixVersion)++".hrl",
    {ok, L} = epp:parse_file(IncludeDir++FileName,[IncludeDir],[]),
    Fields = lists:flatten(lists:filtermap(fun(X) -> 
        case X of
            {attribute,_,type,{{record, RecordName}, Fs,_}} -> 
                {true, Fs};
            _E -> false end end, L)),
    N = lists:filtermap(fun(Y) -> case Y of
                                      {record_field,_,{atom,_,RgrName}, 
                                       {cons,_,{record,_,RgrName,[]},_}} ->
                                          {true,
                                            {fieldset, [{id, atom_to_list(RgrName)},
                                                        {name, atom_to_list(RgrName)}],
                                             get_records_fields2(FixVersion, RgrName)}
                                          };
                                      {typed_record_field,{record_field,_,{atom,_,FieldName}},
                                       {type,_,union,[{atom,_,undefined},
                                                      {type,_,record,[{atom,_,FieldName}]}]}} ->
                                          {true,
                                                {fieldset, [{id, atom_to_list(FieldName)},
                                                        {name, atom_to_list(FieldName)}],
                                             get_records_fields2(FixVersion, FieldName)} 
                                          };
                                      {typed_record_field,{record_field,_,{atom,_,FieldName}},
                                       {type,_,union,[_,{type,_,Type,[]}]}} ->
                                          F = atom_to_list(FieldName),
                                          {true, get_field_ehtml(F, Type, L, RecordName)};
                                      {typed_record_field,{record_field,_,{atom,_,FieldName}},Type} ->
                                          F = atom_to_list(FieldName),
                                          {true, get_field_ehtml(F, Type, L, RecordName)};
                                      _Else -> false
                                  end end, Fields),
    
      lists:flatten(N).

get_field_ehtml(N, Type, Def, RecordName) ->
    Name = lists:concat([RecordName, "|", N]),
    case lists:filter(fun(X) -> case X of {attribute,_,type,{Type,_,_}} -> true; _ -> false end  end, Def) of
        [{attribute,_,type,{Type,{type,_,union,L},_}}] ->
            Options = [{option, [{value, ""}],""}|[{option, [{value, atom_to_list(X)}],atom_to_list(X)} || {atom,_,X}<- L]],
            [{p, [], N},
             {select,[{name,Name}],Options},{input,[{name,Name}, {type, "text"}],[]}];
        _Else ->
           [{p, [], N}, {input,[{name,Name}, {type, "text"}],[]}]
    end.


calculate_reply(L) ->
    Reply = calculate_reply(L, undefined, []),
    generate_reply(Reply).

calculate_reply([{"next","finish"}|[]], Agg, ToReturn) ->
    [Agg|ToReturn];
calculate_reply([{Name, Value}|L], Agg, ToReturn) ->
    case string:str(Name, "incoming message") == 1 of
        true -> 
            case Agg of
                undefined -> calculate_reply(L, #reply{in = Value}, ToReturn);
                E -> calculate_reply(L, #reply{in = Value}, [E|ToReturn])
            end;
        false -> case string:str(Name, "outcoming message") == 1 of
                     true -> 
                         calculate_reply(L, Agg#reply{out = Value}, ToReturn);
                     false ->
                         RecordName = Agg#reply.out,
                         case string:tokens(Name, "|") of
                             [RecordName, FieldName] ->
                                 calculate_reply(L, Agg#reply{fields = [{FieldName, Value}|Agg#reply.fields]}, ToReturn);
                             [RecordName2, FieldName] ->
                                calculate_reply(L, Agg#reply{fields = [{RecordName2, FieldName, Value}|Agg#reply.fields]}, ToReturn)
                         end
                 end
    end.

generate_reply(L) ->
    generate_reply(L, "").

generate_reply([], Agg) ->
    Agg;
generate_reply([#reply{in = In, out = Out, fields = Fields}|R], Agg) ->
    Aggg = generate_reply("reply({#"++In++"{}, NsF}, _SeqNumIn, _SeqNumOut) ->\n"++
                   "    Reply0 = #"++Out++"{},\n",
                   Out, Fields, 1),
    generate_reply(R, Agg ++ Aggg).
                   
generate_reply(Agg, _Out, [], _C) ->
    lists:sublist(Agg, erlang:length(Agg)-2) ++ ";\n";
generate_reply(Agg, Out, [Field|Fields], C) ->
    case Field of
        {Name, Value} ->
            generate_reply(lists:concat([Agg, "    Reply", C, " = Reply", C-1,"#", Out, "{", Name, " = ", Value, "},\n"]),
                           Out, Fields, C+1);
        {RecName, Name, Value} -> 
            generate_reply(lists:concat([Agg, "    Reply", C, " = Reply", C-1, "#", Out, "{", 
                                         RecName, "= Reply", C-1, "#", Out, ".", RecName, "#", RecName,"{",Name, " = ", Value,  "}},\n"]),
                           Out, Fields, C+1)
     end.