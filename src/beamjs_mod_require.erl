-module(beamjs_mod_require).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
	{ok, Cwd} = file:get_cwd(),
	erlv8_fun:new(fun require/2,?V8Obj([
										{"__doc__", "*require* provides a way to load another javascript modules. Not fully compatible with [CommonJS Modules/1.1](http://wiki.commonjs.org/wiki/Modules/1.1) yet,"
										 "but eventually [will be](https://github.com/beamjs/beamjs/issues/issue/3)"},
										{"paths", ?V8Arr([Cwd])}])).

require(#erlv8_fun_invocation{ vm = VM } = Invocation, [Filename]) ->
	case application:get_env(beamjs,available_mods) of
		{ok, Mods} ->
			case proplists:get_value(Filename,Mods) of
				undefined ->
					require_file(Invocation, Filename);
				Mod -> %% it is an Erlang-implemented module
					Mod:exports(VM)
			end;
		_ ->
			require_file(Invocation, Filename)
	end.

require_file(#erlv8_fun_invocation{ vm = VM } = Invocation, Filename) ->
	Global = Invocation:global(),
	Require = Global:get_value("require"),
	RequireObject = Require:object(),
	Paths = RequireObject:get_value("paths",erlv8_vm:taint(VM,?V8Arr(["."]))),
	Sources = lists:filter(fun (not_found) -> 
								 false;
							 (_) ->
								 true
						 end,
						 lists:map(fun (Path) ->
										   case file:read_file(filename:join([Path, Filename ++ ".js"])) of
											   {error, _} ->
												   not_found;
											   {ok, B} ->
												   binary_to_list(B)
										   end
								   end,
								   Paths:list())),
	case Sources of 
		[] ->
			{throw, {error, lists:flatten(io_lib:format("Cannot find module '~s'",[Filename])) }};
		[S|_] ->
			{ok, NewVM} = erlv8_vm:new(),	
			beamjs:load_default_mods(NewVM),
			NewGlobal = erlv8_vm:global(NewVM),
			case erlv8_vm:run(NewVM,S) of
				{ok, _Result} ->
					NewGlobal:get_value("exports",?V8Arr([]));
				{exception, Exception} ->
					{throw, Exception};
				{compilation_failed, Exception} ->
					{throw, Exception};
				_ ->
					ignore_for_now
			end
	end.
	
	

