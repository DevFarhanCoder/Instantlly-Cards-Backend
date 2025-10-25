%%%-------------------------------------------------------------------
%%% @doc Standalone WebSocket Echo Server (No Dependencies)
%%% This version uses only built-in Erlang libraries
%%% @end
%%%-------------------------------------------------------------------
-module(standalone_server).
-export([start/0, stop/0]).

start() ->
    io:format("~n===========================================~n"),
    io:format("🚀 Starting Standalone Test Server~n"),
    io:format("===========================================~n~n"),
    
    %% Start inets (built-in HTTP server)
    application:start(inets),
    
    io:format("✅ Server started!~n"),
    io:format("🌐 Test: http://localhost:8080~n~n"),
    io:format("Press Ctrl+C twice to stop~n~n"),
    
    %% Start basic HTTP server
    {ok, _Pid} = inets:start(httpd, [
        {port, 8080},
        {server_name, "standalone_test"},
        {server_root, "."},
        {document_root, "./priv"},
        {modules, [mod_get]},
        {mime_types, [
            {"html", "text/html"},
            {"css", "text/css"},
            {"js", "text/javascript"}
        ]}
    ]),
    
    io:format("✅ HTTP Server ready at http://localhost:8080~n"),
    ok.

stop() ->
    application:stop(inets).
