-module(beamjs_mod_repl).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	erlv8_object:new([{"start", fun start/2}]).

start(#erlv8_fun_invocation{} = Invocation, []) ->
	case node() of
		nonode@nohost ->
			start(Invocation,["beam.js> "]);
		Node ->
			start(Invocation,[lists:flatten(io_lib:format("(~p)beam.js> ",[Node]))])
	end;
			
start(#erlv8_fun_invocation{ vm = VM } = _Invocation, [Prompt]) ->
	supervisor:start_child(beamjs_repl_sup,[Prompt, beamjs_repl_console, VM]),
	receive X ->
			X 
	end.
