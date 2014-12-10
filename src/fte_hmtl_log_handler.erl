-module(fte_hmtl_log_handler).

%% Export for websocket callbacks
-export([handle_message/1]).

handle_message({text, <<"stop">>}) ->
    lager:info("stop log handler", []),
    {close, normal};

handle_message({text, <<"client-connected">>}) ->
    lager:info("log handler start", []),
    {reply, {text,<<"ok">>}};

handle_message({text, Message}) ->
    lager:info("log handler got text ~p~n", [Message]),
    R = test_archive_worker:subscribe(erlang:binary_to_list(Message)),
    {reply, {text,R}};

handle_message({binary, Message}) ->
    lager:info("log handler got ~p~n", [Message]),
    {reply, {binary, Message}};

handle_message({close, Status, Reason}) ->
    lager:info("log handler closed ~p / ~p~n", [Status, Reason]),
    {close, Status}.

