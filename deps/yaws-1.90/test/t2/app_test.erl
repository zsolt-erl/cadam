-module(app_test).
-include("../include/tftest.hrl").
-include("../ibrowse/src/ibrowse.hrl").
-compile(export_all).


%% Way to invoke just one test
start([F]) ->
    ?line {ok, _} = ibrowse:start_link(),
    apply(app_test, F, []),
    ibrowse:stop().

start() ->
    ?line ok,
    ?line {ok, _} = ibrowse:start_link(),
    server_options_test(),
    test1(),
    test2(),
    test3(),
    appmod_test(),
    streamcontent_test(),
    sendfile_get(),
    json_test(),
    ibrowse:stop().


test1() ->
    io:format("test1\n",[]),
    L = lists:seq(1, 100),
    SELF = self(),
    Pids = lists:map(fun(I) ->
			     spawn(fun() -> slow_client(I, SELF) end)
		     end, L),
    ?line ok = allow_connects(Pids, 5),
    ?line ok = collect_pids(Pids).


collect_pids([]) ->
    ok;
collect_pids(Pids) ->
    receive
	{Pid, done} ->
	    collect_pids(lists:delete(Pid, Pids))
    end.


%% max 5 connectors at a time
allow_connects([], _) ->
    io:format("  test1: all pids connected \n",[]);
allow_connects(Pids, 0) ->
    receive
	{Pid, connected} ->
	    allow_connects(lists:delete(Pid, Pids), 1)
    end;
allow_connects(Pids, I) ->
    receive
	{Pid, allow} ->
	    Pid ! allow,
	    allow_connects(Pids, I-1);
	{Pid, connected} ->
	    allow_connects(lists:delete(Pid, Pids), I+1)
    end.



slow_client(I, Top) ->
    Top ! {self(), allow},
    receive
	allow ->
	    ok
    end,
    ?line {ok, C} = gen_tcp:connect(localhost, 8000, [{active, false},
						      {packet, http}]),
    Top ! {self(), connected},
    ?line ok = gen_tcp:send(C, "GET /1000.txt HTTP/1.1\r\n"
			    "Host: localhost:8000\r\n\r\n"),
    ?line {ok, Sz} = get_cont_len(C),
    ?line ok = inet:setopts(C, [binary,{packet, 0}]),
    read_loop(C, I, Sz),
    Top ! {self(), done}.

read_loop(C, _I, Sz) when Sz - 200000 < 0 ->
    ?line ok = gen_tcp:close(C),
    ok;
read_loop(C, I, Sz)  ->
    ?line {ok, B} = gen_tcp:recv(C, 0),
    %%io:format("(I=~p Sz=~p) ", [I, size(B)]),
    timer:sleep(2),
    read_loop(C, I, Sz - size(B)).


get_cont_len(C) ->
    ?line {value, {http_header, _,_,_, LenStr}} =
	lists:keysearch('Content-Length', 3, tftest:get_headers(C)),
    {ok, list_to_integer(LenStr)}.




test2() ->
    io:format("test2\n",[]),
    blkget(10).


blkget(0) ->
     ok;
blkget(I) ->
    spawn(fun() ->
                  {ok, C} = gen_tcp:connect(localhost, 8000, [{active, false}]),
                  gen_tcp:send(C, "GET /2000.txt HTTP/1.1\r\n"
                                  "Host: localhost:8000\r\n\r\n"),
                  gen_tcp:recv(C, 200),
                  timer:sleep(5000),
                  exit(normal)
          end),
    blkget(I-1).




test3() ->
    io:format("test3\n",[]),
    ?line {ok, "200", _Headers, []} = ibrowse:send_req("http://localhost:8000",
                                                       [], head),
    ok.

server_options_test() ->
    io:format("server_options_test\n",[]),
    {ok, S} = gen_tcp:connect("localhost", 8000, [{packet, raw}, list,
                                                  {active, false}]),
    ok = gen_tcp:send(S, "OPTIONS * HTTP/1.1\r\nHost: localhost\r\n\r\n"),
    inet:setopts(S, [{packet, http}]),
    ?line ok = server_options_recv(S),
    gen_tcp:close(S).

server_options_recv(S) ->
    do_server_options_recv(S, []).
server_options_recv(_S, [http_eoh|_]) ->
    ok;
server_options_recv(S, [{http_response,{1,1},200,"OK"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Server',_,_}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Allow',_,"GET, HEAD, OPTIONS, PUT, POST, DELETE"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Content-Length',_,"0"}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(S, [{http_header,_,'Date',_,_}|_]=Hdrs) ->
    do_server_options_recv(S, Hdrs);
server_options_recv(_S, [Hdr|_Hdrs]) ->
    io:format("unexpected server options HTTP header: ~p~n", [Hdr]),
    error.
do_server_options_recv(S, Hdrs) ->
    {ok, Hdr} = gen_tcp:recv(S, 0, 5000),
    server_options_recv(S, [Hdr|Hdrs]).

-define(SENDFILE_GET_TIMEOUT, 120000).

sendfile_get() ->
    io:format("sendfile_get\n",[]),
    L = lists:seq(1, 5),
    SELF = self(),
    K1 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} =
                                       ibrowse:send_req(
                                         "http://localhost:8000/1000.txt",
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k1, done}
                           end)
             end, L),

    K2 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} =
                                       ibrowse:send_req(
                                         "http://localhost:8000/2000.txt",
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k2, done}
                           end)
             end, L),

    K3 = lists:map(
             fun(_) ->
                     spawn(fun() ->
                                   ?line {ok, "200", _Headers, _} =
                                       ibrowse:send_req(
                                         "http://localhost:8000/3000.txt",
                                         [], get, [], [], ?SENDFILE_GET_TIMEOUT),
                                   SELF ! {self(), k3, done}
                           end)
             end, L),


    collect(K1, 1, k1),
    collect(K2, 1, k2),
    collect(K3, 1, k3),
    io:format("\n",[]),
    ok.

collect([], _, _) ->
    ok;
collect(L, Count, Tag) ->
    receive
        {Pid, Tag, done} ->
            io:format("(~p ~p)", [Tag, Count]),
            collect(lists:delete(Pid, L), Count+1, Tag)
    after ?SENDFILE_GET_TIMEOUT ->
            io:format("TIMEOUT ~p\n~p~n",[process_info(self()), L]),
            ?line exit(timeout)
    end.

-define(APPMOD_HEADER, "Appmod-Called").

appmod_test() ->
    io:format("appmod_test\n",[]),
    Uri1 = "http://localhost:8002/",
    ?line {ok, "200", Headers1, _} = ibrowse:send_req(Uri1, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers1),
    Uri2 = "http://localhost:8003/",
    ?line {ok, "200", Headers2, _} = ibrowse:send_req(Uri2, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers2),
    Uri3 = "http://localhost:8003/icons/layout.gif",
    ?line {ok, "200", Headers3, _} = ibrowse:send_req(Uri3, [], get),
    ?line false = proplists:get_value(?APPMOD_HEADER, Headers3, false),
    Uri4 = "http://localhost:8004/non_root_appmod",
    ?line {ok, "200", Headers4, _} = ibrowse:send_req(Uri4, [], get),
    ?line "true" = proplists:get_value(?APPMOD_HEADER, Headers4),
    ok.

streamcontent_test() ->
    io:format("streamcontent_test\n",[]),
    Uri1 = "http://localhost:8000/streamtest/1",
    ?line {ok, "200", Headers1, Body1} = ibrowse:send_req(Uri1, [], get),
    ?line "chunked" = proplists:get_value("Transfer-Encoding", Headers1),
    ?line Body1 = "this is an iolist",

    %% The following test attempts to ensure that Yaws doesn't report the
    %% following problem due to the application closing the socket and then
    %% handing it back to Yaws via stream_process_end.
    %%
    %% =ERROR REPORT==== 12-May-2010::00:27:05 ===
    %% Yaws process died: {{badmatch,{error,einval}},
    %%                     [{yaws,setopts,3},
    %%                      {yaws,http_recv_request,2},
    %%                      {yaws,do_http_get_headers,2},
    %%                      {yaws,http_get_headers,2},
    %%                      {yaws_server,aloop,3},
    %%                      {yaws_server,acceptor0,2},
    %%                      {proc_lib,init_p_do_apply,3}]}
    %%
    %% The test uses plain sockets because closing the remote end makes
    %% ibrowse unhappy. Unfortunately the only way to currently check that
    %% the above message doesn't appear is to turn on traffic tracing in
    %% yaws.conf and then visually check the file logs/report.log.
    %%
    Path = "/streamtest/2",
    {ok, Sock} = gen_tcp:connect("localhost", 8000, [binary, {active, false}]),
    gen_tcp:send(Sock, "GET " ++ Path ++ " HTTP/1.1\r\nHost: localhost\r\n\r\n"),
    inet:setopts(Sock, [{packet, http}]),
    {ok, Len} = recv_hdrs(Sock),
    inet:setopts(Sock, [{packet, raw}, {active, false}]),
    {ok, <<"closing the socket">>} = gen_tcp:recv(Sock, Len),
    timer:sleep(10000),
    gen_tcp:close(Sock),
    ok.

json_test() ->
    io:format("json_test\n",[]),
    io:format("  param array1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {array, [42, 23]}},
                                 {"id", 1}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 1}]}),
    io:format("  param array2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {array, [23, 42]}},
                                 {"id", 2}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", -19},
                                 {"id", 2}]}),
    io:format("  param obj1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {struct, [{"subtrahend", 23},
                                                      {"minuend", 42}]}},
                                 {"id", 3}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 3}]}),
    io:format("  param obj2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "subtract"},
                                 {"params", {struct, [{"minuend", 42},
                                                      {"subtrahend", 23}]}},
                                 {"id", 4}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"result", 19},
                                 {"id", 4}]}),
    io:format("  notif1\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "update"},
                                 {"params", {array, [1,2,3,4,5]}}]},
                       notification),
    io:format("  notif2\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "foobar"}]},
                       notification),
    io:format("  missing method\n", []),
    ?line ok = do_json({struct, [{"jsonrpc", "2.0"},
                                 {"method", "foobar"},
                                 {"id", "1"}]},
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", "1"},
                                 {"error", {struct,
                                            [{"code", -32601},
                                             {"message", "method not found"}]}
                                  }]}),
    io:format("  invalid json\n", []),
    InvalidJson = "{\"jsonrpc\": \"2.0\", \"method\": \"foobar,"
        "\"params\": \"bar\", \"baz]",
    ?line ok = do_json(InvalidJson,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32700},
                                             {"message", "parse error"}]}
                                  }]},
                       no_encode),
    io:format("  invalid req1\n", []),
    InvalidReq1 = "{\"jsonrpc\": \"2.0\", \"method\": 1, \"params\": \"bar\"}",
    ?line ok = do_json(InvalidReq1,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32600},
                                             {"message", "invalid request"}]}
                                  }]},
                       no_encode),
    io:format("  invalid params\n", []),
    InvalidReq2 = "{\"jsonrpc\": \"2.0\", \"method\": \"x\","
        "\"params\": \"bar\"}",
    ?line ok = do_json(InvalidReq2,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32602},
                                             {"message", "invalid params"}]}
                                  }]},
                       no_encode),
    io:format("  invalid batch json\n", []),
    InvalidJsonBatch = "[ {\"jsonrpc\": \"2.0\", \"method\": \"sum\","
        "\"params\": [1,2,4],\"id\": \"1\"},{\"jsonrpc\": \"2.0\", \"method\" ]",
    ?line ok = do_json(InvalidJsonBatch,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32700},
                                             {"message", "parse error"}]}
                                  }]},
                       no_encode),
    io:format("  empty batch\n", []),
    EmptyBatch = "[]",
    ?line ok = do_json(EmptyBatch,
                       {struct, [{"jsonrpc", "2.0"},
                                 {"id", null},
                                 {"error", {struct,
                                            [{"code", -32600},
                                             {"message", "invalid request"}]}
                                  }]},
                       no_encode),
    io:format("  invalid batch1\n", []),
    BogusBatch1 = "[1]",
    ?line ok = do_json(BogusBatch1,
                       {array,
                        [{struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]}]},
                       no_encode),
    io:format("  invalid batch2\n", []),
    BogusBatch2 = "[1,2,3]",
    ?line ok = do_json(BogusBatch2,
                       {array,
                        [{struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]},
                         {struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]},
                         {struct, [{"jsonrpc", "2.0"},
                                   {"id", null},
                                   {"error", {struct,
                                              [{"code", -32600},
                                               {"message", "invalid request"}]}
                                   }]}]},
                       no_encode),
    io:format("  mixed batch\n", []),
    MixedBatch = "
    [{\"jsonrpc\":\"2.0\",\"method\":\"sum\",\"params\": [1,2,4], \"id\": \"1\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"notify_hello\", \"params\": [7]},
     {\"jsonrpc\":\"2.0\",\"method\":\"subtract\",\"params\":[42,23],
      \"id\":\"2\"},
     {\"foo\": \"boo\"},
     {\"jsonrpc\":\"2.0\",\"method\":\"foo.get\",
      \"params\":{\"name\": \"myself\"}, \"id\": \"5\"},
     {\"jsonrpc\": \"2.0\", \"method\": \"get_data\", \"id\": \"9\"}]",
    ?line ok = do_json(MixedBatch,
                       {array,
                        [{struct,[{"jsonrpc","2.0"},
                                  {"result",7},
                                  {"id","1"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"result",19},{"id","2"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"error",
                                   {struct,[{"code",-32600},
                                            {"message","invalid request"}]}},
                                  {"id",null}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"error",
                                   {struct,[{"code",-32601},
                                            {"message","method not found"}]}},
                                  {"id","5"}]},
                         {struct,[{"jsonrpc","2.0"},
                                  {"result",{array,["hello",5]}},
                                  {"id","9"}]}]},
                       no_encode),
    io:format("  all-notification batch\n", []),
    NotifBatch = "[
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_sum\", \"params\": [1,2,4]},
        {\"jsonrpc\": \"2.0\", \"method\": \"notify_hello\", \"params\": [7]}]",
    ?line ok = do_json(NotifBatch, notification, no_encode),
    ok.

do_json(Req, Expected) ->
    do_json(Req, Expected, encode).
do_json(Req, notification, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    ?line [] = Body,
    ok;
do_json(Req, {struct, _}=Expected, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    check_json(Expected, Body, true);
do_json(Req, {array, Array}, NeedEncode) ->
    ?line {ok, "200", Headers, Body} = json_send(Req, NeedEncode),
    ?line "application/json" = proplists:get_value("Content-Type", Headers),
    ?line {ok, {array, GotArray}} = json2:decode_string(Body),
    lists:map(fun({Obj, Got}) ->
                      ?line ok = check_json(Obj, Got, false)
              end, lists:zip(Array, GotArray)),
    ok.

check_json({struct, _}=Exp, Body, true) ->
    ?line {ok, DecodedBody} = json2:decode_string(Body),
    check_json(Exp, DecodedBody);
check_json({struct, _}=Exp, Body, false) ->
    check_json(Exp, Body).
check_json({struct, Members}, DecodedBody) ->
    lists:foreach(fun({Key, Val}) ->
                          ?line Val = jsonrpc:s(DecodedBody, Key)
                  end, Members),
    ok.

json_send(Req) ->
    json_send(Req, encode).
json_send(Req, encode) ->
    json_send(json2:encode(Req), no_encode);
json_send(Req, no_encode) ->
    Uri = "http://localhost:8005/jsontest",
    ReqHdrs = [{content_type, "application/json"}],
    ibrowse:send_req(Uri, ReqHdrs, post, Req).

recv_hdrs(Sock) ->
    recv_hdrs(Sock, 0).
recv_hdrs(Sock, Len) ->
    inet:setopts(Sock, [{active, once}]),
    receive
        {http, Sock, http_eoh} ->
            {ok, Len};
        {http, Sock, {http_error, Error}} ->
            {error, Error};
        {http, Sock, {http_header, _, 'Content-Length', _, LenStr}} ->
            recv_hdrs(Sock, list_to_integer(LenStr));
        {http, Sock, {http_header, _, _, _, _}} ->
            recv_hdrs(Sock, Len);
        {http, Sock, {http_response, _, 200, "OK"}} ->
            recv_hdrs(Sock, Len);
        Other ->
            {error, {"unexpected message", Other}}
    end.

%% used for appmod tests
%%
out(_A) ->
    %% add our special header to mark that we were here
    [{status, 200},
     {header, {?APPMOD_HEADER, "true"}}].
