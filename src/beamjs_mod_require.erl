-module(beamjs_mod_require).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	fun require/3.

require(Script, #erlv8_fun_invocation{} = Invocation, [Filename]) ->
	case application:get_env(beamjs,available_mods) of
		{ok, Mods} ->
			case proplists:get_value(Filename,Mods) of
				undefined ->
					require_file(Script, Invocation, Filename);
				Mod -> %% it is an Erlang-implemented module
					Mod:exports()
			end;
		_ ->
			require_file(Script, Invocation, Filename)
	end.

require_file(_Script, #erlv8_fun_invocation{} = _Invocation, Filename) ->
	{ok, B} = file:read_file(Filename),
	S = binary_to_list(B),
	{ok, NewScript} = erlv8_script:new(),	
	beamjs:load_default_mods(NewScript),
	case erlv8_script:run(NewScript,S) of
		{ok, _Result} ->
			proplists:get_value("exports",erlv8_script:global(NewScript),[]);
		_Other ->
			error(not_implemented)
	end.
	
	

