-module(beamjs_js_formatter).
-export([format/2,format_exception/2]).

format(Script,Expr) when is_list(Expr) ->
	Expr1 = lists:flatten(io_lib:format("~p",[Expr])),
	Detail = erlv8_script:to_detail_string(Script,Expr),
	case ("\"" ++ Detail ++ "\"") of
		Expr1 -> %% it is an actual string
			"\"" ++ Expr ++ "\"";
		_ ->
			"[" ++ lists:flatten(string:join(lists:map(fun (I) ->
															   format(Script, I)
													   end, Expr), ", ")) ++ "]"
	end;

format(_Script,undefined) ->
	"";

format(Script,{erlv8_object, _}=O) ->
	"{" ++	lists:flatten(string:join(lists:map(fun ({K,V}) ->
														format(Script, K) ++ ":" ++ format(Script, V)
												end, O:proplist()), ", ")) ++ "}";

format(Script,Expr) ->
	erlv8_script:to_detail_string(Script,Expr).

format_exception(_Script,Exception) ->
	lists:flatten(
	  io_lib:format("  ~s~n",[Exception:get_value("stack")])).




