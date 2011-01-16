-module(beamjs_mod_erlv8).
-behaviour(beamjs_module).
-export([init/1,exports/1]).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"nextTick",fun next_tick/2},
			{"enqueueTick", fun enqueue_tick/2},
			{"gc", fun gc/2}
		   ]).

next_tick(#erlv8_fun_invocation{ vm = VM },[#erlv8_fun{ resource = R }=F]) ->
	spawn(fun () -> erlv8_vm:next_tick(VM, {call, R, []}) end),
	F.

enqueue_tick(#erlv8_fun_invocation{ vm = VM },[#erlv8_fun{ resource = R }=F]) ->
	spawn(fun () -> erlv8_vm:enqueue_tick(VM, {call, R, []}) end),
	F.

gc(#erlv8_fun_invocation{ vm = VM },[]) ->
	erlv8_vm:gc(VM).
	
