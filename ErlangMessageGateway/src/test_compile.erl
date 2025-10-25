% Quick compile and test script
% Run this with: erl -noshell -s test_compile run -s init stop

-module(test_compile).
-export([run/0]).

run() ->
    io:format("~n===========================================~n"),
    io:format("Testing Erlang Compilation~n"),
    io:format("===========================================~n~n"),
    
    % Compile simple_test module
    case compile:file(simple_test, [debug_info, return_errors]) of
        {ok, _} ->
            io:format("✅ Compilation successful!~n"),
            io:format("✅ Testing module...~n~n"),
            simple_test:start(),
            simple_test:hello();
        {error, Errors, Warnings} ->
            io:format("❌ Compilation failed!~n"),
            io:format("Errors: ~p~n", [Errors]),
            io:format("Warnings: ~p~n", [Warnings])
    end,
    io:format("~n").
