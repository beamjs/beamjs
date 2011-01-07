-module(beamjs_mod_erlang).
-behaviour(beamjs_module).
-export([init/1,exports/1]).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(VM) ->
	PidProto = erlv8_extern:get_proto(VM, pid),
	PidProto:set_value("toString", fun (#erlv8_fun_invocation{ this = This }, []) -> "[pid " ++ pid_to_list(This) ++ "]" end), %% I know erlang documentation says we shouldn't use it, but I wonder how terribly I'll be punished for this?
	RefProto = erlv8_extern:get_proto(VM, ref),
	RefProto:set_value("toString", fun (#erlv8_fun_invocation{ this = This }, []) -> "[ref " ++ erlang:ref_to_list(This) ++ "]" end), %% Same goes to this
	AtomProto = erlv8_extern:get_proto(VM, atom),
	AtomProto:set_value("toString", fun (#erlv8_fun_invocation{ this = This }, []) -> atom_to_list(This) end),
	PortProto = erlv8_extern:get_proto(VM, port),
	PortProto:set_value("toString", fun (#erlv8_fun_invocation{ this = This }, []) -> "[port " ++ erlang:port_to_list(This) ++ "]" end), %% And this
	BinProto = erlv8_extern:get_proto(VM, bin),
	BinProto:set_value("toString", fun (#erlv8_fun_invocation{ this = This }, []) -> "[binary " ++ lists:flatten(io_lib:format("~p",[This])) ++ "]" end),

	?V8Obj([{"apply", fun erlang_apply/2},
			{"atom", fun new_atom/2}, {"tuple", fun new_tuple/2}, {"binary", fun new_binary/2}
		   ]).

new_atom(#erlv8_fun_invocation{ vm = VM, is_construct_call = ICC }, [Atom]) when is_list(Atom) ->
	case ICC of
		true ->
			{throw, {error, "atom() can't be used as a constructor"}};
		false ->
			erlv8_extern:extern(VM, list_to_atom(Atom))
	end.

new_tuple(#erlv8_fun_invocation{ vm = VM, is_construct_call = ICC }, [#erlv8_array{}=Array]) ->
	case ICC of
		true ->
			{throw, {error, "tuple() can't be used as a constructor"}};
		false ->
			erlv8_extern:extern(VM, list_to_tuple(Array:list()))
	end.

new_binary(#erlv8_fun_invocation{ vm = VM, is_construct_call = ICC }, [#erlv8_array{}=Array]) ->
	case ICC of
		true ->
			{throw, {error, "binary() can't be used as a constructor"}};
		false ->
			erlv8_extern:extern(VM, list_to_binary(Array:list()))
	end;

new_binary(#erlv8_fun_invocation{ vm = VM, is_construct_call = ICC }, [List]) when is_list(List) ->
	case ICC of
		true ->
			{throw, {error, "binary() can't be used as a constructor"}};
		false ->
			erlv8_extern:extern(VM, list_to_binary(List))
	end.

erlang_apply(#erlv8_fun_invocation{ vm = VM }, [Module, Fun, #erlv8_array{} = Args]) when is_list(Module) andalso is_list(Fun) ->
	from_native(VM, erlang:apply(list_to_atom(Module), list_to_atom(Fun), to_native(VM, Args))).

-define(Is(O,X),  ((O:get_prototype()):get_value("constructor")):equals(erlv8_vm:retr(VM, {?MODULE, X}))).

to_native(VM, #erlv8_object{}=O) ->
	lists:map(fun ({K,V}) ->
					  {K, to_native(VM, V)}
			  end, O:proplist());

to_native(VM, #erlv8_array{}=A) ->
	lists:map(fun (V) -> to_native(VM, V) end, A:list());

to_native(_VM, Other) ->
	Other.

from_native(VM, T) when is_tuple(T) ->
	erlv8_extern:extern(VM, T);

from_native(VM, A) when is_atom(A) ->
	erlv8_extern:extern(VM, A);
	
from_native(VM, P) when is_port(P) ->
	erlv8_extern:extern(VM, P);

from_native(VM, L) when is_list(L) ->
	case is_simple_proplist(L) of
		true ->
			?V8Obj(lists:map(fun ({K,V}) ->
									 {K, from_native(VM, V)}
							 end, L));
		false ->
			case io_lib:printable_list(L) of
				true ->
					L;
				false ->
					?V8Arr(lists:map(fun (V) -> from_native(VM, V) end, L))
			end
	end;

from_native(_VM, Other) ->
	Other.

	
is_simple_proplist([{K,_}]) when is_list(K) orelse is_atom(K) ->
       true;
is_simple_proplist([{K,_}|Y]) when is_list(K) orelse is_atom(K) ->
       is_simple_proplist(Y);
is_simple_proplist([_|_]) ->
       false;
is_simple_proplist([]) ->
       false.


