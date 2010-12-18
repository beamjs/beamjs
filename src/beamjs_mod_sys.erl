-module(beamjs_mod_sys).
-export([exports/0,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports() ->
	?V8Obj([{"print", fun print/2}]).

print(#erlv8_fun_invocation{ vm = VM} = _Invocation, [Expr]) ->
	io:format("~s",[erlv8_vm:to_detail_string(VM,Expr)]),
	undefined.
