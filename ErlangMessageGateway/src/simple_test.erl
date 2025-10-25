%%%-------------------------------------------------------------------
%%% @doc Simple test application to verify Erlang is working
%%% @end
%%%-------------------------------------------------------------------
-module(simple_test).
-export([start/0, hello/0]).

start() ->
    io:format("~n~n===========================================~n"),
    io:format("✅ Erlang is working!~n"),
    io:format("===========================================~n~n"),
    io:format("Your Erlang version: ~s~n", [erlang:system_info(otp_release)]),
    io:format("System time: ~p~n~n", [erlang:system_time(millisecond)]),
    ok.

hello() ->
    io:format("Hello from Erlang! 🚀~n"),
    {ok, "Erlang is ready!"}.
