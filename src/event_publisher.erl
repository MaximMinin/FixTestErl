-module(event_publisher).
-behaviour(gen_server).

-include("../deps/yaws/include/yaws_api.hrl").

%% API
-export([out/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
          sock,
          yaws_pid,
          messages =[]
         }).

out(A) ->
    case (A#arg.req)#http_request.method of
        'GET' ->
            case yaws_api:get_header(A#arg.headers, accept) of
                undefined ->
                    {status, 406};
                Accept ->
                    case string:str(Accept, "text/event-stream") of
                        0 ->
                            {status, 406};
                        _ ->
                            {ok, Pid} = gen_server:start(?MODULE, [A], []),
                            yaws_sse:headers(Pid)
                    end
            end;
        _ ->
            [{status, 405},
             {header, {"Allow", "GET"}}]
    end.

init([Arg]) ->
    process_flag(trap_exit, true),
    {ok, #state{sock=Arg#arg.clisock}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ok, YawsPid},#state{sock=S}= State) ->
    split_server:register(quote), %%TODO
    {noreply, State#state{yaws_pid=YawsPid}};
handle_info({discard, _YawsPid}, State) ->
    %% nothing to do
    {stop, normal, State};
handle_info({out, M}, #state{sock=Socket, messages = Messages}=State) ->
    NewMessages = lists:sublist([{"<-", M}|Messages], 50),
    sendData(Socket, NewMessages, State#state{messages = NewMessages});
handle_info({in, M}, #state{sock=Socket, messages = Messages}=State) ->
    NewMessages = lists:sublist([{"->", M}|Messages], 50),
    sendData(Socket, NewMessages, State#state{messages = NewMessages});
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State#state{sock=closed}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{sock=Socket, yaws_pid=YawsPid}) ->
    yaws_api:stream_process_end(Socket, YawsPid),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

sendData(Socket, List, State) ->
    Da = lists:concat(lists:map(fun({Direction, Reg}) -> io_lib:format("~s ~s~n", [Direction, Reg]) end, List)),
    Data = yaws_sse:data(Da, [trim]),
    case yaws_sse:send_events(Socket, Data) of
        ok ->
            {noreply, State};
        {error, closed} ->
            {stop, normal, State};
        {error, Reason} ->
            {stop, Reason, State}
    end.
