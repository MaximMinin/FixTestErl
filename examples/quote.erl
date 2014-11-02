%% Author: Maxim Minin
%% Created: 28.05.2012
%% Description: TODO: Add description to quoteRequest
-module(quote).

-behaviour(test_case_template).
%%
%% Include files
%%
-include_lib("fix_convertor/include/FIX_4_2.hrl").
%%
%% Exported Functions
%%
-export([reply/3, ini/0, get_mod/0, get_ip/0, get_port/0, get_fix_version/0, sendQuote/1]).

%%
%% API Functions
%%
get_mod() ->
    client.
get_port() ->
    12345.
get_fix_version() ->
    'FIX 4.2'.
get_ip() ->
    localhost.


reply(#quote{standardHeader = #standardHeader{msgSeqNum =999}},  _SeqNumIn, _SeqNumOut)->
    io:format("Quote - END: ~p~n", [erlang:now()]),
    ok;
reply(#heartbeat{} = H, _SeqNumIn, _SeqNumOut)->
    io:format("Quote - Heartbeat: ~p~n", [H]),
    [#heartbeat{ standardHeader = #standardHeader{beginString = auto,
                                                          msgType = heartbeat,
                                                          bodyLength = auto,
                                                          senderCompID = <<"TEST_ONE">>,
                                                  targetCompID = <<"TEST_TWO">>
                                                },
                standardTrailer = #standardTrailer{checkSum=auto}
               }];
reply(_Msg,  _SeqNumIn, _SeqNumOut)->
%%    io:format("Quote - Msg: ~p~n", [Msg]),
    ok.

ini()->
    spawn(?MODULE, sendQuote, [2]),
    [#logon{
            resetSeqNumFlag = yes,
            standardTrailer = #standardTrailer{
                                               checkSum = auto                                               
                                               },
            standardHeader = #standardHeader{
                                             beginString = auto,
                                             bodyLength = auto,
                                             msgType = logon,
                                             msgSeqNum = 1,
                                             senderCompID = <<"TEST_ONE">>,
                                             targetCompID = <<"TEST_TWO">>
                                             }
           }
    ].
%%
%% Local Functions
%%
sendQuote(1000)->
    io:format("END: ~p ~n", [erlang:now()]),
    ok;
sendQuote(Number)->
timer:sleep(5000),
test_case_worker:send(?MODULE, [#quoteRequest{standardHeader = #standardHeader{beginString = auto, 
                                                                               bodyLength=auto,
                                                                               msgType = quoteRequest,
                                                                               senderCompID = <<"TEST_ONE">>,
                                                                               targetCompID = <<"TEST_TWO">>,
                                                                               msgSeqNum = Number,
                                                                               sendingTime = helper:getNow()},
                                  quoteReqID = helper:getIniq(),
                                  rgr_quoteRequest_146 = [#rgr_quoteRequest_146{symbol = <<"TEST">>,
                                                                                   securityID = <<"TEST">>}],
                                                 standardTrailer = #standardTrailer{checkSum = auto}
                                                 }
                                   ]),
    sendQuote(Number+1).
