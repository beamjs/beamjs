-module(beamjs_mod_erlang).
-behaviour(beamjs_module).
-export([init/1,exports/1]).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"apply", fun erlang_apply/2}]).

erlang_apply(#erlv8_fun_invocation{}, [Module, Fun, #erlv8_array{} = Args]) when is_list(Module) andalso is_list(Fun) ->
	from_native(erlang:apply(list_to_atom(Module), list_to_atom(Fun), to_native(Args))).
	
to_native(#erlv8_object{}=O) ->
	lists:map(fun ({K,V}) ->
					  {K, to_native(V)}
			  end, O:proplist());
to_native(#erlv8_array{}=A) ->
	lists:map(fun to_native/1, A:list());
to_native(Other) ->
	Other.

from_native(T) when is_tuple(T) ->
	?V8Arr(tuple_to_list(T));
from_native(L) when is_list(L) ->
	case is_simple_proplist(L) of
		true ->
			?V8Obj(lists:map(fun ({K,V}) ->
									 {K, from_native(V)}
							 end, L));
		false ->
			?V8Arr(lists:map(fun from_native/1, L))
	end;
from_native(Other) ->
	Other.

	
is_simple_proplist([{K,_}]) when is_list(K) orelse is_atom(K) ->
       true;
is_simple_proplist([{K,_}|Y]) when is_list(K) orelse is_atom(K) ->
       is_simple_proplist(Y);
is_simple_proplist([_|_]) ->
       false;
is_simple_proplist([]) ->
       false.


