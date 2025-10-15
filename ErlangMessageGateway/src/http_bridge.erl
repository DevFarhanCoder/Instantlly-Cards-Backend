%%%-------------------------------------------------------------------
%%% @doc HTTP API Bridge for Node.js Backend Integration
%%% Provides REST endpoints for Node.js to interact with Erlang gateway
%%% @end
%%%-------------------------------------------------------------------
-module(http_bridge).
-export([start/0, stop/0]).

start() ->
    io:format("~n🌉 Starting HTTP Bridge for Node.js integration...~n"),
    
    %% Start inets application
    application:start(inets),
    
    %% Start HTTP server on port 8081
    {ok, _Pid} = inets:start(httpd, [
        {port, 8081},
        {server_name, "erlang_bridge"},
        {server_root, "."},
        {document_root, "./priv"},
        {modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_get, mod_head, mod_log, mod_disk_log]},
        {bind_address, {127,0,0,1}},
        {erl_script_alias, {"/api", [http_bridge_handler]}},
        {mime_types, [
            {"html", "text/html"},
            {"json", "application/json"}
        ]}
    ]),
    
    io:format("✅ HTTP Bridge started on http://localhost:8081/api~n"),
    io:format("   Node.js can now call Erlang gateway via HTTP~n~n"),
    ok.

stop() ->
    inets:stop(httpd, {127,0,0,1, 8081}).
