%% Author: Maxim Minin
%% Created: 28.05.2012
%% Description: TODO: Add description to quoteRequest
-module(quote).

-behaviour(test_case_template).
%%
%% Include files
%%
-include("FIX_4_2.hrl").
%%
%% Exported Functions
%%
-export([reply/3, ini/0, sendQuote/1]).

%%
%% API Functions
%%
reply(#quote{standardHeader = #standardHeader{msgSeqNum =999}},  _SeqNumIn, _SeqNumOut)->
    io:format("Quote - END: ~p~n", [erlang:now()]),
    ok;
reply(#heartbeat{} = H, _SeqNumIn, _SeqNumOut)->
%%     io:format("Quote - Heartbeat: ~p~n", [H]),
    [#heartbeat{ standardHeader = #standardHeader{beginString = auto,
                                                          msgType = heartbeat,
                                                          bodyLength = auto,
                                                          senderCompID = <<"TEST_ONE">>,
                                                  targetCompID = <<"TEST_TWO">>
                                                },
                standardTrailer = #standardTrailer{checkSum=auto}
               }];
reply(Msg,  _SeqNumIn, _SeqNumOut)->
%%     io:format("Quote - Msg: ~p~n", [Msg]),
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
%%     timer:sleep(5000),
    test_case_worker:send(?MODULE, [#quoteRequest{standardHeader = #standardHeader{beginString = auto, 
                                                                                   bodyLength=auto,
                                                                                   msgType = quoteRequest,
                                                                                   senderCompID = <<"TEST_ONE">>,
                                                                                   targetCompID = <<"TEST_TWO">>,
                                                                                   msgSeqNum = Number,
                                                                                   sendingTime = helper:getNow()},
                                                  quoteReqID = helper:getIniq(),
                                                  repeatingReg_quoteRequest_146 = [#repeatingReg_quoteRequest_146{symbol = <<"TEST">>,
                                                                                   securityID = <<"TEST">>}],
                                                 standardTrailer = #standardTrailer{checkSum = auto}
                                                 }
                                   ]),
    sendQuote(Number+1).

