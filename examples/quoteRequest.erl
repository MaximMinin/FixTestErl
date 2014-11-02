%% Author: Maxim Minin
%% Created: 28.05.2012
%% Description: TODO: Add description to quoteRequest
-module(quoteRequest).
-behaviour(test_case_template).
%%
%% Include files
%%
-include_lib("fix_convertor/include/FIX_4_2.hrl").
%%
%% Exported Functions
%%
-export([reply/3, ini/0, get_mod/0, get_ip/0, get_port/0, get_fix_version/0]).

%%
%% API Functions
%%
get_mod() ->
    server.
get_port() ->
    12345.
get_fix_version() ->
    'FIX 4.2'.
get_ip() ->
    localhost.
ini()->
[
     #logon{standardHeader = #standardHeader{beginString = auto,
                                                      bodyLength = auto,
                                                      msgType = logon,
                                                      senderCompID = <<"TEST_TWO">>,
                                                      targetCompID = <<"TEST_ONE">>,
                                                      msgSeqNum = 1,
                                                      senderSubID = <<"Prod_1">>,
                                                      sendingTime = helper:getNow()},
                   encryptMethod = none,
                   heartBtInt = 30,
                   standardTrailer = #standardTrailer{checkSum = auto} 
                   }
 
 ].

reply(M, I, O) ->
    io:format("QuoteRequestWorker get message: ~p~n", [M]),
    reply_local(M,I,O).
%%
%% Local Functions
%%

%% LOGON -> ok
%% <<"8=FIX.4.2",1,"9=67",1,"35=A",1,"49=TEST_TWO",1,"56=TEST_ONE",1,"34=18",1,
%% "52=20120529-14:42:25.795",1,"98=0",1,"108=30",1,"10=048",1>>.
reply_local(#logon{standardHeader = #standardHeader{senderCompID = <<"TEST_ONE">>, 
                                              targetCompID = <<"TEST_TWO">> }}, _In, _Out) ->
    ok;
reply_local(#logon{standardHeader = #standardHeader{senderCompID = Sender}}, SeqNumIn, _Out) ->
    [#logout{text = <<"Sender unknown">>,
            standardHeader = #standardHeader{beginString = auto,
                                             bodyLength = auto,
                                             msgType = quote,
                                             msgSeqNum = SeqNumIn +1,
                                             sendingTime = helper:getNow(),
                                             senderCompID = <<"TEST_TWO">>,
                                             targetCompID = Sender,
                                             senderSubID = <<"Prod_1">>},
            standardTrailer = #standardTrailer{checkSum = auto}
             }
    ];
%% <<"8=FIX.4.2",1,"9=55",1,"35=5",1,"49=TEST_TWO",1,"56=TEST_ONE",1,
%% "34=19",1,"52=20120529-14:42:29.619",1,"10=008",1>>
reply_local(#logout{}, _In, _Out) ->
    ok;
%% QUOTEREQUSET -> QUOTE
%%
%% <<"8=FIX.4.2",1,"9=190",1,"35=S",1,"49=TEST_TWO",1,"56=TEST_ONE",1,"34=27",1,"50=Prod_1",1,
%%  "52=20120529-14:44:59.831",1,"131=136c5f825ea",1,"117=QuoteId795",1,
%% "55=DE0007093353",1,"48=DE0007093353",1,"22=4",1,"132=5.31",1,"133=5.33",1,
%% "134=1000",1,"135=1000",1,"62=20120529-14:45:59",1,"10=219",1>>.
reply_local(#quoteRequest{quoteReqID = QuoteReqID, 
                    rgr_quoteRequest_146 = [#rgr_quoteRequest_146{symbol = Isin,
                                                                                   securityID = Wkn}]}, 
      SeqNumIn, _SeqNumOut)->
    [#quote{
            validUntilTime = helper:getNow(30),
            bidPx = 10.0,
            bidSize = 10000,
            offerPx = 11.0,
            offerSize = 10000,
            iDSource = iSINNumber,
            securityID = Wkn,
            symbol = Isin,
            quoteID = helper:getIniq(),
            quoteReqID = QuoteReqID,
            standardHeader = #standardHeader{beginString = auto,
                                             bodyLength = auto,
                                             msgType = quote,
                                             msgSeqNum = SeqNumIn +1,
                                             sendingTime = helper:getNow(),
                                             senderCompID = <<"TEST_TWO">>,
                                             targetCompID = <<"TEST_ONE">>,
                                             senderSubID = <<"Prod_1">>},
            standardTrailer = #standardTrailer{checkSum = auto}
            }];

%% ORDER -> ORDERACK
%% <<"8=FIX.4.2",1,"9=262",1,"35=8",1,"49=TEST_TWO",1,"56=TEST_ONE",1,
%% "34=28",1,"57=Prod_1",1,"52=20120529-14:45:02.984",1,"37=OrderID796",1,
%% "11=TEST_TWOQuoteId771",1,"17=ExecID797",1,"20=0",1,"150=2",1,
%% "39=2",1,"55=DE0007093353",1,"48=DE0007093353",1,"22=4",1,
%% "207=F",1,"54=1",1,"38=100",1,"40=D",1,"59=6",1,
%% "432=20120418",1,"32=0",1,"31=0",1,"151=100",1,"14=0",1,"6=0",1,
%% "60=20120529-14:45:02.984",1,"21=1",1,"10=254",1>>
reply_local(#orderSingle{quoteID = QuoteId, clOrdID = OrderId}, SeqNumIn, _SeqNumOut) ->
    [
     #executionReport{
                      clOrdID = OrderId,
                      orderID = todo,
            standardHeader = #standardHeader{beginString = auto,
                                             bodyLength = auto,
                                             msgType = quote,
                                             msgSeqNum = SeqNumIn +1,
                                             sendingTime = helper:getNow(),
                                             senderCompID = <<"TEST_TWO">>,
                                             targetCompID = <<"TEST_ONE">>,
                                             senderSubID = <<"Prod_1">>},
            standardTrailer = #standardTrailer{checkSum = auto}
                      
                      }
     ];

%%HEARTBEAT
%%
%% <<""8=FIX.4.2",1,"9=55",1,"35=0",1,"49=TEST_TWO",1,"56=TEST_ONE",1,
%% "34=21",1,"52=20120529-14:42:36.235",1,"10=244",1>>.
reply_local(#testRequest{testReqID = TestReqID}, SeqNumIn, _SeqNumOut) ->
  [
     #heartbeat{standardHeader = #standardHeader{beginString = auto, 
                                                          bodyLength = auto, 
                                                          msgType = heartbeat,
                                                          senderCompID = <<"TEST_TWO">>,
                                                          targetCompID = <<"TEST_ONE">>,
                                                          msgSeqNum = SeqNumIn +1,
                                                          sendingTime = helper:getNow()}, 
                       testReqID = TestReqID, 
                       standardTrailer = #standardTrailer{checkSum = auto}}
   ];

%%SESSIONLEVELREJECT
%%
%% <<"8=FIX.4.2",1,"9=63",1,"35=3",1,"49=TEST_TWO",1,"56=TEST_ONE",1,"34=23",1,
%% "52=20120529-14:42:43.780",1,"45=1872",1,"10=116",1>>.

reply_local(_Msg, SeqNumIn, SeqNumOut)->
    [
     #reject{
             refSeqNum = SeqNumOut,
             standardHeader = #standardHeader{beginString = auto,
                                              bodyLength = auto,
                                              msgType = reject,
                                              msgSeqNum = SeqNumIn+1,
                                              senderCompID = <<"TEST_TWO">>,
                                              targetCompID = <<"TEST_ONE">>,
                                              sendingTime = helper:getNow()},
             standardTrailer = #standardTrailer{checkSum = auto}
             }
     ].
