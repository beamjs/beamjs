-module(beamjs_js_formatter).
-export([format/2,format_exception/2]).

format(_VM,Expr) when is_list(Expr) ->
	lists:flatten(io_lib:format("~p",[Expr]));

format(_VM,undefined) ->
	"";

format(VM,{erlv8_array, _, _}=A) ->
	"[" ++ lists:flatten(string:join(lists:map(fun (I) ->
													   format(VM, I)
											   end, A:list()), ", ")) ++ "]";

format(VM,{erlv8_object, _, _}=O) ->
	"{" ++	lists:flatten(string:join(lists:map(fun ({K,V}) ->
														format(VM, K) ++ ":" ++ format(VM, V)
												end, O:proplist()), ", ")) ++ "}";

format(VM,Expr) ->
	erlv8_vm:to_detail_string(VM,Expr).

format_exception(_VM,Exception) ->
	lists:flatten(
	  io_lib:format("  ~s~n",[Exception:get_value("stack")])).




