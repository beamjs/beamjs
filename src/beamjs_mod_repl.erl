-module(beamjs_mod_repl).
-export([exports/1,init/1]).
-behaviour(beamjs_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"start", fun start/2},
			{"quit", fun quit/2}]).

start(#erlv8_fun_invocation{} = Invocation, []) ->
	case node() of
		nonode@nohost ->
			start(Invocation,["beam.js> "]);
		Node ->
			start(Invocation,[lists:flatten(io_lib:format("(~p)beam.js> ",[Node]))])
	end;
			
start(#erlv8_fun_invocation{ vm = VM } = I, [Prompt]) ->
	Ref = make_ref(),
	{ok, Pid} = supervisor:start_child(beamjs_repl_sup,[Prompt, beamjs_repl_console, VM]),
	erlv8_vm:stor(VM, {?MODULE, Ref}, self()),
	Global = I:global(),
	Global:set_hidden_value("replSessionRef", Ref),
	receive X ->
			exit(Pid),
			X 
	end.

quit(#erlv8_fun_invocation{ vm = VM} = I, []) ->
	Global = I:global(),
	Ref = Global:get_hidden_value("replSessionRef"),
	Pid = erlv8_vm:retr(VM, {?MODULE, Ref}),
	Pid ! ok.
