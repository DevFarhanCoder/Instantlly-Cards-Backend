%%%-------------------------------------------------------------------
%%% @doc Simple HTTP/WebSocket Server using built-in httpd
%%% This version works without external dependencies
%%% @end
%%%-------------------------------------------------------------------
-module(simple_gateway).
-export([start/0, stop/0, start_link/0]).

start() ->
    start_link().

start_link() ->
    io:format("~n~n===========================================~n"),
    io:format("🚀 Starting Simple Message Gateway~n"),
    io:format("===========================================~n~n"),
    
    %% Start core components
    io:format("Starting core services...~n"),
    
    {ok, _Pid1} = message_router:start_link(),
    io:format("✅ Message Router started~n"),
    
    {ok, _Pid2} = session_manager:start_link(),
    io:format("✅ Session Manager started~n"),
    
    {ok, _Pid3} = presence_tracker:start_link(),
    io:format("✅ Presence Tracker started~n"),
    
    %% Start WebSocket server
    {ok, _Pid4} = simple_websocket_server:start_link(),
    io:format("✅ WebSocket Server started~n"),
    
    io:format("~n===========================================~n"),
    io:format("✅ Message Gateway Core is RUNNING!~n"),
    io:format("===========================================~n~n"),
    
    io:format("📊 System Status:~n"),
    io:format("  - Message routing: ACTIVE~n"),
    io:format("  - Session tracking: ACTIVE~n"),
    io:format("  - Presence tracking: ACTIVE~n~n"),
    
    io:format("🧪 Test Commands:~n"),
    io:format("  session_manager:add_session(<<\"user1\">>, <<\"device1\">>, self()).~n"),
    io:format("  presence_tracker:user_online(<<\"user1\">>).~n"),
    io:format("  presence_tracker:get_presence(<<\"user1\">>).~n~n"),
    
    {ok, self()}.

stop() ->
    ok.
