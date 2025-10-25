%%%-------------------------------------------------------------------
%%% @doc message_gateway top level application module
%%% @end
%%%-------------------------------------------------------------------
-module(message_gateway_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("~n~n===========================================~n"),
    io:format("Starting Erlang Message Gateway...~n"),
    io:format("===========================================~n~n"),
    
    %% Start the supervisor
    case message_gateway_sup:start_link() of
        {ok, Pid} ->
            %% Start Cowboy HTTP server
            start_cowboy(),
            io:format("✅ Message Gateway started successfully!~n"),
            io:format("🌐 WebSocket endpoint: ws://localhost:8080/ws~n"),
            io:format("🏥 Health check: http://localhost:8080/health~n~n"),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    io:format("~nStopping Message Gateway...~n"),
    cowboy:stop_listener(http_listener),
    ok.

%% Internal functions
start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/ws", ws_handler, []},
            {"/health", health_handler, []},
            {"/", cowboy_static, {file, "priv/index.html"}}
        ]}
    ]),
    
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ).
