-module(beamjs).

-export([start/0,stop/0,main/1]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

args(_,[]) ->
	ok;
args(Script,["-sname",Node|Rest]) ->
	net_kernel:start([list_to_atom(Node),shortnames]),
	args(Script,Rest);
args(Script,["-name",Node|Rest]) ->
	net_kernel:start([list_to_atom(Node),longnames]),
	args(Script,Rest);
args(Script,["-toolbar"|Rest]) ->
	toolbar:start(),
	args(Script,Rest);

args(Script,[File|Rest]) when is_list(File) ->
	{ok, B} = file:read_file(File),
	S = binary_to_list(B),
	erlv8_script:source(Script,S),
	erlv8_script:run(Script),
	args(Script,Rest).
	
main(Args) ->
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			os:putenv("ERLV8_SO_PATH","./deps/erlv8/priv")
	end,
	erlv8:start(),
	start(),
	{ok, Script} = erlv8:new_script(""),
	args(Script,Args),
	supervisor:start_child(beamjs_repl_sup,["beam.js> ", beamjs_repl_console, Script]),
	receive 
		_ ->
			ok
	end.

	
