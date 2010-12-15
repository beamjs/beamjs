-module(beamjs_js_formatter).
-export([format/2,format_exception/2]).

format(Script,Expr) when is_list(Expr) ->
	Expr1 = lists:flatten(io_lib:format("~p",[Expr])),
	Detail = erlv8_script:to_detail_string(Script,Expr),
	case ("\"" ++ Detail ++ "\"") of
		Expr1 -> %% it is an actual string
			"\"" ++ Expr ++ "\"";
		_ ->
			%% check if it is a proplist
			case is_proplist(Expr) of
				true ->
					"{" ++	lists:flatten(string:join(lists:map(fun ({K,V}) ->
															format(Script, K) ++ ":" ++ format(Script, V)
																end, Expr), ", ")) ++ "}";
				false ->
					"[" ++ lists:flatten(string:join(lists:map(fun (I) ->
																	   format(Script, I)
															   end, Expr), ", ")) ++ "]"
			end
	end;

format(_Script,undefined) ->
	"";

format(Script,Expr) ->
	erlv8_script:to_detail_string(Script,Expr).

format_exception(_Script,Exception) ->
	lists:flatten(
	  io_lib:format("  ~s~n",[proplists:get_value("stack",Exception)])).


is_proplist([{_,_}]) ->
	true;
is_proplist([{_,_}|Y]) ->
	is_proplist(Y);
is_proplist([_|_]) ->
	false;
is_proplist([]) ->
	false.




