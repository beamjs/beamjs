-module(beamjs_bundle).
-export([from_app_env/0,load/2, unload/2, modules/1, modules/2,bundles/0, add_bundle/1, remove_bundle/1, set_bundles/1]).
-include_lib("erlv8/include/erlv8.hrl").

from_app_env() ->
	case application:get_env(beamjs,available_bundles) of
		{ok, BL} ->
			lists:map(fun ({B, _}) -> B end, BL);
		_ ->
			[]
	end.

add_bundle(Bundle) ->
	application:set_env(beamjs, bundles, lists:usort([default,runtime,Bundle|bundles()])).

remove_bundle(Bundle) ->
	application:set_env(beamjs, bundles, lists:usort([default,runtime|bundles()] -- [Bundle])).

set_bundles(Bundles) ->
	application:set_env(beamjs, bundles, lists:usort([default,runtime|Bundles])).


load(VM, Bundle) when is_list(Bundle) ->
	load(VM, list_to_atom(Bundle));
load(VM, Bundle) when is_atom(Bundle) ->
	Global = erlv8_vm:global(VM),
	Global:set_value("module",?V8Obj([{"id","init"},{"exports", ?V8Obj([])}])),
	add_bundle(Bundle),
	lists:foreach(fun({Name, Module}) ->
						  case beamjs_mod_require:require(VM, Module) of
							  {throw, _} = Throw ->
								  {error, Throw};
							  Exports ->
								  Global:set_value(Name, Exports)
						  end
				  end,  modules(globals, Bundle)),
	Bundle.

unload(VM, Bundle) when is_list(Bundle) ->
	unload(VM, list_to_atom(Bundle));
unload(VM, Bundle) when is_atom(Bundle) ->
	Global = erlv8_vm:global(VM),
	remove_bundle(Bundle),
	lists:foreach(fun({Name, _}) ->
						  Global:delete(Name)
				  end,  modules(globals, Bundle)),
	Bundle.

	
modules(Type,Bundle) ->
	case application:get_env(beamjs,available_bundles) of
		{ok, MB} ->
			proplists:get_value(Type, proplists:get_value(Bundle, MB, []), []);
		_ ->
			[]
	end.
	
modules(Type) ->
	lists:flatten(lists:map(fun (B) -> modules(Type, B) end, bundles())).


bundles() ->
		case application:get_env(beamjs, bundles) of
			{ok, Bundles0} ->
				Bundles0;
			_ ->
				[default,runtime]
		end.
