-module(beamjs_mod_console).
-export([exports/1,init/1]).
-behaviour(beamjs_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	%% TODO: time, timeEnd, trace, assert
	erlv8_object:new([{"__doc__", "CommonJS [Console](http://wiki.commonjs.org/wiki/Console) Implementation"},
			{"log", fun log/2},{"info", fun log/2},{"warn", fun log/2},{"error", fun log/2},{"dir", fun dir/2}]).

log(#erlv8_fun_invocation{ vm = VM } = _Invocation, [Expr]) ->
	io:format("~s~n",[beamjs_js_formatter:format(VM,Expr)]),
	undefined.

dir(#erlv8_fun_invocation{ vm = VM } = _Invocation, [#erlv8_object{}=Expr]) ->
	lists:foreach(fun ({Prop, Value}) ->
						  io:format("  ~s: ~s~n",[Prop,beamjs_js_formatter:format(VM,Value)])
				  end, Expr:proplist());
dir(#erlv8_fun_invocation{} = Invocation, [#erlv8_fun{}=Expr]) ->
	dir(Invocation, [Expr:object()]).

