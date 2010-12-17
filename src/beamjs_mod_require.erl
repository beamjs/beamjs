-module(beamjs_mod_require).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	{ok, Cwd} = file:get_cwd(),
	erlv8_funobj:new(fun require/2,
					 [{"paths", [Cwd]}]).

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
	Require = proplists:get_value("require", Invocation:global()),
	[S|_] = lists:map(fun (Path) ->
									  case file:read_file(filename:join([Path, Filename])) of
										  {error, _} ->
											  not_found;
										  {ok, B} ->
											  binary_to_list(B)
									  end
							  end,
							  proplists:get_value("paths",Require:object(),".")),
	case S of 
		undefined ->
			undefined;
		_ ->
			{ok, NewScript} = erlv8_script:new(),	
			beamjs:load_default_mods(NewScript),
			case erlv8_script:run(NewScript,S) of
				{ok, _Result} ->
					proplists:get_value("exports",erlv8_script:global(NewScript),[]);
				_Other ->
					error(not_implemented)
			end
	end.
	
	

