-module(beamjs_mod_require).
-export([exports/1,init/1]).

-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

-define(PREAMBLE, "(function (exports, __filename, __dirname) { ").
-define(POSTAMBLE, "})").

init(_VM) ->
	ok.

exports(VM) ->
	{ok, Cwd} = file:get_cwd(),
	Paths = erlv8_vm:taint(VM,?V8Arr([Cwd])),
	Paths:set_value("__doc__","Array of paths where require() will be looking for modules"),
	erlv8_fun:new(fun require/2,?V8Obj([
										{"__doc__", 
										 "*require* provides a way to load another javascript modules. Not fully compatible with [CommonJS Modules/1.1](http://wiki.commonjs.org/wiki/Modules/1.1) yet, "
										 "but eventually [will be](https://github.com/beamjs/beamjs/issues/issue/3)." ++ [10] ++
										 "<h3>Synopsis</h3>"
										 "require(ModuleId)"
										},
										{"paths", Paths}])).

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

require_file(#erlv8_fun_invocation{ this = This, vm = VM } = Invocation, Filename) ->
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
												   {Path, Filename ++ ".js", binary_to_list(B)}
										   end
								   end,
								   Paths:list())),
	case Sources of 
		[] ->
			{throw, {error, lists:flatten(io_lib:format("Cannot find module '~s'",[Filename])) }};
		[{Path,LoadedFilename,S}|_] ->
			ModuleObject = erlv8_vm:taint(VM,?V8Obj([{"exports",?V8Obj([])}])),
			case erlv8_vm:run(VM,?PREAMBLE ++ S ++ ?POSTAMBLE,{LoadedFilename,0,-length(?PREAMBLE)}) of
				{ok, F} ->
					F:call(This, [ModuleObject:get_value("exports"), filename:join([Path,LoadedFilename]), Path]),
					ModuleObject:get_value("exports");
				{_,_} ->
					ignore_for_now
			end
	end.
	
	

