-module(beamjs_js_formatter).
-export([format/2,format_exception/2]).

format(VM,Expr) when is_list(Expr) ->
	Expr1 = lists:flatten(io_lib:format("~p",[Expr])),
	Detail = erlv8_vm:to_detail_string(VM,Expr),
	case ("\"" ++ Detail ++ "\"") of
		Expr1 -> %% it is an actual string
			"\"" ++ Expr ++ "\"";
		_ ->
			"[" ++ lists:flatten(string:join(lists:map(fun (I) ->
															   format(VM, I)
													   end, Expr), ", ")) ++ "]"
	end;

format(_VM,undefined) ->
	"";

format(VM,{erlv8_object, _}=O) ->
	"{" ++	lists:flatten(string:join(lists:map(fun ({K,V}) ->
														format(VM, K) ++ ":" ++ format(VM, V)
												end, O:proplist()), ", ")) ++ "}";

format(VM,Expr) ->
	erlv8_vm:to_detail_string(VM,Expr).

format_exception(_VM,Exception) ->
	lists:flatten(
	  io_lib:format("  ~s~n",[Exception:get_value("stack")])).




