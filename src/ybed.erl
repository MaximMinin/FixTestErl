-module(ybed).
-compile(export_all).

start() ->
    {ok, spawn(?MODULE, run, [])}.

run() ->
    Id = "embedded",
    GconfList = [
                 {id, Id}
                ],
    {ok, [{Ip,_,_}|_]} = inet:getif(),   
    Docroot = "../../www",
    SconfList = [
                 {port, 8888},
                 {servername, "fixTestErl"},
                 {listen, Ip},
                 {docroot, Docroot},
                 {appmods, [
                            {"/sse", event_publisher},
                            {"/start/", fix_start},
                            {"/stop/", fix_stop}
                           ]}
                ],
    {ok, SCList, GC, ChildSpecs} =
        yaws_api:embedded_start_conf(Docroot, SconfList, GconfList, Id),
    [supervisor:start_child(ybed_sup, Ch) || Ch <- ChildSpecs],
    yaws_api:setconf(GC, SCList),
    {ok, self()}.
