-module(beamjs_mod_console).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	[{"log", fun log/3},{"info", fun log/3},{"warn", fun log/3},{"error", fun log/3},{"dir", fun log/3}
	 %% TODO: time, timeEnd, trace, assert
	].

log(Script, #erlv8_fun_invocation{} = _Invocation, [Expr]) ->
	io:format("~s~n",[beamjs_js_formatter:format(Script,Expr)]),
	undefined.

