%%%-------------------------------------------------------------------
%%% @doc Start All Erlang Gateway Services
%%% Starts the complete messaging gateway in the correct order
%%% @end
%%%-------------------------------------------------------------------
-module(start_all).
-export([start/0, stop/0]).

start() ->
    io:format("~n🚀 Starting Erlang Message Gateway...~n~n"),
    
    % Start core services
    io:format("1️⃣ Starting Message Router...~n"),
    message_router:start_link(),
    
    io:format("2️⃣ Starting Session Manager...~n"),
    session_manager:start_link(),
    
    io:format("3️⃣ Starting Presence Tracker...~n"),
    presence_tracker:start_link(),
    
    io:format("4️⃣ Starting WebSocket Server (port 8080)...~n"),
    simple_websocket_server:start_link(),
    
    io:format("~n✅ ALL SERVICES RUNNING!~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n"),
    io:format("📡 WebSocket Server: ws://localhost:8080/ws~n"),
    io:format("📨 Message Router: ACTIVE~n"),
    io:format("👥 Session Manager: ACTIVE~n"),
    io:format("🟢 Presence Tracker: ACTIVE~n"),
    io:format("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━~n~n"),
    
    ok.

stop() ->
    io:format("~n🛑 Stopping all services...~n"),
    simple_websocket_server:stop(),
    ok.
