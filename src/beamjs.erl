-module(beamjs).

-export([start/0,stop/0,main/1]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

args([]) ->
	ok;
args(["-net"|Rest]) ->
	net_kernel:start([beamjs,shortnames]),
	args(Rest);
args(["-toolbar"|Rest]) ->
	toolbar:start(),
	args(Rest);
args([File|Rest]) ->
	{ok, B} = file:read_file(File),
	S = binary_to_list(B),
	{ok, Script} = erlv8:new_script(S),
	erlv8_script:run(Script),
	args(Rest).

	
main(Args) ->
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			os:putenv("ERLV8_SO_PATH","./deps/erlv8/priv")
	end,
	erlv8:start(),
	args(Args),
	start(),
	supervisor:start_child(beamjs_repl_sup,["beam.js> ", beamjs_repl_console]),
	receive 
		_ ->
			ok
	end.

	
