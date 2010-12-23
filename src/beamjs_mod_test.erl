-module(beamjs_mod_test).
-export([exports/1,init/1]).

-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"run", fun run/2}]).

run(#erlv8_fun_invocation{ vm = VM }=I, [#erlv8_object{}=Tests]) ->
	lists:foreach(fun ({"test" ++ Name, #erlv8_fun{}=Test}) when length(Name) > 0 ->
						  io:format("\e[0m~n Test: ~s~n",[Name]),
						  case Test:call() of
							  {throw, E} ->
								  case E of
									  E when is_list(E) ->
										  io:format("     \e[31mfailed: ~s\e[0m~n",[E]);
									  {error, #erlv8_object{}=Exc} ->
										  io:format("     \e[31mfailed: ~s\e[0m~n",[beamjs_js_formatter:format(VM, Exc)])
								  end;
							  _ ->
								  io:format("    \e[32mpassed\e[0m~n")
						  end;
					  ({"test" ++ Name, #erlv8_object{}=TestsObj}) when length(Name) > 0 ->
						  run(I,[TestsObj]);
					  (_) ->
						  ignore
				  end, Tests:proplist()).

