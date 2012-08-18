-module(ybed).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [
                 {id, Id}
                ],
    Docroot = "../../www",
    SconfList = [
                 {port, 8888},
                 {servername, "fixTestErl"},
                 {listen, {127,0,0,1}},
                 {docroot, Docroot},
                 {appmods, [
                            {"/sse", event_publisher}
                           ]}
                ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
