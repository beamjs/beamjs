-module(beamjs_mod_erlang).
-behaviour(beamjs_module).
-export([init/1,exports/1]).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(VM) ->
	PidProto = erlv8_extern:get_pid_proto(VM),
	PidProto:set_value("toString", fun (#erlv8_fun_invocation{}, []) -> "[pid]" end),
	RefProto = erlv8_extern:get_ref_proto(VM),
	RefProto:set_value("toString", fun (#erlv8_fun_invocation{}, []) -> "[ref]" end),
	Atom = erlv8_vm:taint(VM, fun new_Atom/2),
	erlv8_vm:stor(VM, {?MODULE, atom}, Atom),
	Tuple = erlv8_vm:taint(VM, fun new_Tuple/2),
	erlv8_vm:stor(VM, {?MODULE, tuple}, Tuple),
	?V8Obj([{"apply", fun erlang_apply/2},
			{"Atom", Atom}, {"Tuple", Tuple}
		   ]).

new_Atom(#erlv8_fun_invocation{ this = This }, [Atom]) when is_list(Atom) ->
	This:set_value("atom", Atom).

new_Tuple(#erlv8_fun_invocation{ this = This }, [#erlv8_array{}=Array]) ->
	This:set_value("tuple", Array).

erlang_apply(#erlv8_fun_invocation{ vm = VM }, [Module, Fun, #erlv8_array{} = Args]) when is_list(Module) andalso is_list(Fun) ->
	from_native(VM, erlang:apply(list_to_atom(Module), list_to_atom(Fun), to_native(VM, Args))).

-define(Is(O,X),  ((O:get_prototype()):get_value("constructor")):equals(erlv8_vm:retr(VM, {?MODULE, X}))).

to_native(VM, #erlv8_object{}=O) ->
	case ?Is(O,atom) of
		true ->
			list_to_atom(O:get_value("atom"));
		false ->
			case ?Is(O,tuple) of
				true ->
					list_to_tuple(lists:map(fun (V) -> to_native(VM, V) end, (O:get_value("tuple")):list()));
				false ->
					lists:map(fun ({K,V}) ->
									  {K, to_native(VM, V)}
							  end, O:proplist())
			end
	end;

to_native(VM, #erlv8_array{}=A) ->
	lists:map(fun (V) -> to_native(VM, V) end, A:list());

to_native(_VM, Other) ->
	Other.

from_native(VM, T) when is_tuple(T) ->
	L = tuple_to_list(T),
	TupleCtor = erlv8_vm:retr(VM, {?MODULE, tuple}),
	TupleCtor:instantiate([?V8Arr(lists:map(fun (V) -> from_native(VM, V) end, L))]);

from_native(VM, A) when is_atom(A) ->
	S = atom_to_list(A),
	AtomCtor = erlv8_vm:retr(VM, {?MODULE, atom}),
	AtomCtor:instantiate([S]);
	
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


