-module(beamjs_mod_sys).
-export([exports/1,init/1]).
-behaviour(beamjs_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"print", fun print/2},
			{"inspect", fun inspect/2}]).

print(#erlv8_fun_invocation{ vm = VM} = _Invocation, [Expr]) ->
	io:format("~s",[erlv8_vm:to_detail_string(VM,Expr)]),
	undefined.

inspect(#erlv8_fun_invocation{ vm = VM} = _Invocation, [Expr]) ->
	lists:flatten(io_lib:format("~s",[beamjs_js_formatter:format(VM, Expr)])).
