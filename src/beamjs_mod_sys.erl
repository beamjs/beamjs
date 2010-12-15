-module(beamjs_mod_sys).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	[{"print", fun print/3}].

print(Script, #erlv8_fun_invocation{} = _Invocation, Expr) ->
	io:format("~s",[erlv8_script:to_detail_string(Script,Expr)]),
	undefined.

