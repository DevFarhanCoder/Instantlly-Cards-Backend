% =========================================
% COMPILE ALL CORE MODULES
% Run this in Erlang shell
% =========================================

% Navigate to src directory
cd("C:/Users/user3/Documents/App/ErlangMessageGateway/src").

% Compile core modules
io:format("~n===========================================~n").
io:format("Compiling Core Modules...~n").
io:format("===========================================~n~n").

io:format("1. Compiling session_manager...~n").
compile:file(session_manager, [{outdir, "../ebin"}, debug_info, return_errors]).

io:format("2. Compiling presence_tracker...~n").
compile:file(presence_tracker, [{outdir, "../ebin"}, debug_info, return_errors]).

io:format("3. Compiling message_router...~n").
compile:file(message_router, [{outdir, "../ebin"}, debug_info, return_errors]).

io:format("4. Compiling message_gateway_sup...~n").
compile:file(message_gateway_sup, [{outdir, "../ebin"}, debug_info, return_errors]).

io:format("~n===========================================~n").
io:format("✅ All Core Modules Compiled!~n").
io:format("===========================================~n~n").

% Add to path
cd("..").
code:add_path("ebin").

io:format("Ready to test! Try:~n").
io:format("  message_gateway_sup:start_link().~n~n").
