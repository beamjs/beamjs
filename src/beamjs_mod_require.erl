-module(beamjs_mod_require).
-export([exports/1,init/1]).

-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(VM) ->
	erlv8_vm:stor(VM,{beamjs_mod_require,mod_tab},ets:new(require_modules,[public,{heir, VM, none}])),
	ok.

exports(VM) ->
	{ok, Cwd} = file:get_cwd(),
	Paths = erlv8_vm:taint(VM,?V8Arr([Cwd])),
	Paths:set_value("__doc__","Array of paths where require() will be looking for modules"),	
	erlv8_fun:new(fun require/2,?V8Obj([
				{"__doc__",
					"`require()` provides a way to load another javascript modules. Not fully compatible with [CommonJS Modules/1.1](http://wiki.commonjs.org/wiki/Modules/1.1) yet, "
					"but eventually [will be](https://github.com/beamjs/beamjs/issues/issue/3)." ++ [10] ++
					"### Synopsis\n"
					"    "
					"    require(ModuleId)"
					"    "
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

file_reader(Path,Filename) ->
	case file_reader(Path,Filename,".js") of
		not_found ->
			file_reader(Path,Filename,"");
		Result ->
			Result
	end.

file_reader(Path,Filename,Ext) ->
	case file:read_file(filename:join([Path, Filename ++ Ext])) of
		{error, _} ->
			not_found;
		{ok, B} ->
			{filename:dirname(filename:absname(Path ++ "/" ++ Filename)), Filename ++ Ext, binary_to_list(B)}
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
						 lists:map(fun (Path) -> file_reader(Path, Filename) end,Paths:list())),
	case Sources of 
		[] ->
			{throw, {error, lists:flatten(io_lib:format("Cannot find module '~s'",[Filename])) }};
		[{Path,LoadedFilename,S}|_] ->
			Tab = erlv8_vm:retr(VM,{beamjs_mod_require,mod_tab}),

			case ets:lookup(Tab,Filename) of
				[{Filename, loading}] ->
					Module = Global:get_value("module"),
					ModuleId = Module:get_value("id"),
					ets:delete(Tab,Filename),
					ets:delete(Tab,ModuleId),
					{throw, {error, lists:flatten(io_lib:format(
													"Dependency cycle detected while attempting to load '~s' from '~s'",
													[Filename, ModuleId]))}};
				[{Filename, Exports}] ->
					io:format("cached already: ~p~n",[Exports]),
					Exports;
				[] ->
					NewCtx = erlv8_context:new(VM),
					NewGlobal = erlv8_context:global(NewCtx),
					lists:foreach(fun ({K,V}) ->
										  NewGlobal:set_value(K,V)
								  end,  Global:proplist()),
					NewGlobal:set_value("require",fun require/2),
					NewRequire = NewGlobal:get_value("require"),
					lists:foreach(fun ({K,V}) ->
										  NewRequire:set_value(K,V)
								  end,  Require:proplist()),
					NewGlobal:set_value("module",?V8Obj([])),
					Module = NewGlobal:get_value("module"),
					Module:set_value("id", Filename, [dontdelete,readonly]),
					NewGlobal:set_value("exports",?V8Obj([])),
					NewGlobal:set_value("__dirname",Path),
					NewGlobal:set_value("__filename",filename:join([Path,LoadedFilename])),

					ets:insert(Tab,{Filename, loading}),

					case erlv8_vm:run(VM,NewCtx,S,{LoadedFilename,0,0}) of
						{ok, _} ->
							lists:foreach(fun ({"exports",_}) ->
												  ignore;
											  ({"__dirname",_}) ->
												  ignore;
											  ({"__filename",_}) ->
												  ignore;
											  ({"module",_}) ->
												  ignore;
											  ({K,V}) ->
												  Global:set_value(K,V)
										  end,  NewGlobal:proplist()),
							Exports = NewGlobal:get_value("exports"),
							ets:insert(Tab, {Filename, Exports}),
							Exports;
						{_,E} ->
							ets:delete(Tab, Filename),
							{throw, {error, E}}
					end
			end
	end.
	
	

