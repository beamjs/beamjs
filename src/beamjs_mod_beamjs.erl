-module(beamjs_mod_beamjs).

-behaviour(erlv8_module).

-export([exports/1, init/1]).

-include_lib("erlv8/include/erlv8.hrl").


init(_VM) ->
	ok.

exports(VM) ->
	VMCtor = erlv8_vm:taint(VM, fun vm_constructor/2),
	VMCtor:set_value("prototype", prototype_VM()),

	Current = VMCtor:instantiate(),
	Current:set_prototype(prototype_VM()),
	Current:set_hidden_value("VMServer",VM),

	VMCtor:set_value("current",Current),
	?V8Obj([{"VM", VMCtor},
			{"bundles",
			 ?V8Obj([{"loaded", fun loaded/2},
					 {"unload", fun unload/2},
					 {"load", fun load/2}])}]).


prototype_VM() ->
	?V8Obj([{"start", fun vm_start/2},
			{"stop", fun vm_stop/2},
			{"run", fun vm_run/2},
			{"runAsync", fun vm_run_async/2},
			{"global", undefined}]).

loaded(#erlv8_fun_invocation{},[]) ->
	?V8Arr(beamjs:bundles()).

unload(#erlv8_fun_invocation{},Bundles) ->
	beamjs:set_bundles(beamjs:bundles() -- lists:map(fun list_to_atom/1, Bundles)),
	?V8Arr(beamjs:bundles()).

load(#erlv8_fun_invocation{},Bundles) ->
	beamjs:set_bundles(beamjs:bundles() ++ lists:map(fun list_to_atom/1, Bundles)),
	?V8Arr(beamjs:bundles()).

vm_constructor(#erlv8_fun_invocation{}, []) ->
	ok.

vm_start(#erlv8_fun_invocation{ this = This }, []) ->
	{ok, VM} = erlv8_vm:start(),
	This:set_hidden_value("VMServer", VM),
	ok.

vm_stop(#erlv8_fun_invocation{ this = This }, []) ->
	case This:get_hidden_value("VMServer") of
		undefined ->
			{throw, {error, "VM is not started"}};
		VM ->
			erlv8_vm:stop(VM),
			This:set_hidden_value("VMServer",undefined),
			ok
	end.

vm_run(#erlv8_fun_invocation{ this = This }, [Code]) when is_list(Code) ->
	case This:get_hidden_value("VMServer") of
		undefined ->
			{throw, {error, "VM is not started"}};
		VM ->
			case erlv8_vm:run(VM, Code) of
				{ok, Result} ->
					Result;
				{compilation_failed, Error} ->
					{throw, Error};
				{exception, Error} ->
					{throw, Error}
			end
	end.

vm_run_async(#erlv8_fun_invocation{ this = This }, [Code, #erlv8_fun{}=Callback]) when is_list(Code) ->
	spawn(fun () ->
				  Result = 
				  case This:get_hidden_value("VMServer") of
					  undefined ->
						  {throw, {error, "VM is not started"}};
					  VM ->
						  case erlv8_vm:run(VM, Code) of
							  {ok, Result0} ->
								  Result0;
							  {compilation_failed, Error} ->
								  {throw, Error};
							  {exception, Error} ->
								  {throw, Error}
						  end
				  end,
				  Callback:call([Result])
		  end),
	ok.

