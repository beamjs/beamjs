-module(beamjs_mod_sys).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	reloader:start().

exports(_VM) ->
	?V8Obj([{"print", fun print/2},
			{"beamjs", ?V8Obj([{"reload", fun reload/2}])}]).

print(#erlv8_fun_invocation{ vm = VM} = _Invocation, [Expr]) ->
	io:format("~s",[erlv8_vm:to_detail_string(VM,Expr)]),
	undefined.

reload(#erlv8_fun_invocation{} = _Invocation, []) ->
	?V8Arr(lists:map(fun ({_,_}=O) -> ?V8Obj([O]) end,reloader:reload_modules(reloader:all_changed()))).
