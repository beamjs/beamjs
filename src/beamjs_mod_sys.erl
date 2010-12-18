-module(beamjs_mod_sys).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	erlv8_object:new([{"print", fun print/2}]).

print(#erlv8_fun_invocation{ vm = VM} = _Invocation, [Expr]) ->
	io:format("~s",[erlv8_vm:to_detail_string(VM,Expr)]),
	undefined.

