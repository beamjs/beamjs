-module(beamjs_mod_require).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	{ok, Cwd} = file:get_cwd(),
	erlv8_fun:new(fun require/2,erlv8_object:new([{"paths", [Cwd]}])).

require(#erlv8_fun_invocation{} = Invocation, [Filename]) ->
	case application:get_env(beamjs,available_mods) of
		{ok, Mods} ->
			case proplists:get_value(Filename,Mods) of
				undefined ->
					require_file(Invocation, Filename);
				Mod -> %% it is an Erlang-implemented module
					Mod:exports()
			end;
		_ ->
			require_file(Invocation, Filename)
	end.

require_file(#erlv8_fun_invocation{} = Invocation, Filename) ->
	Global = Invocation:global(),
	Require = Global:get_value("require"),
	RequireObject = Require:object(),
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
								   RequireObject:get_value("paths","."))),
	case Sources of 
		[] ->
			{throw, {error, lists:flatten(io_lib:format("Cannot find module '~s'",[Filename])) }};
		[S|_] ->
			{ok, NewVM} = erlv8_vm:new(),	
			beamjs:load_default_mods(NewVM),
			Global = erlv8_vm:global(NewVM),
			case erlv8_vm:run(NewVM,S) of
				{ok, _Result} ->
					Global:get_value("exports",[]);
				_Other ->
					error(not_implemented)
			end
	end.
	
	

