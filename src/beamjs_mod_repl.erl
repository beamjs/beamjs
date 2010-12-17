-module(beamjs_mod_repl).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	[{"start", fun start/2}].

start(#erlv8_fun_invocation{} = Invocation, []) ->
	start(Invocation,["beam.js> "]);
start(#erlv8_fun_invocation{ server = Server } = _Invocation, [Prompt]) ->
	supervisor:start_child(beamjs_repl_sup,[Prompt, beamjs_repl_console, Server]),
	receive X ->
			X 
	end.
