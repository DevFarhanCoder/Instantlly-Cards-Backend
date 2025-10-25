% Compile script to run from Erlang shell
cd("C:/Users/user3/Documents/App/ErlangMessageGateway/src").
compile:file(simple_test, [{outdir, "../ebin"}, debug_info, return_errors]).
cd("..").
code:add_path("ebin").
simple_test:start().
simple_test:hello().
