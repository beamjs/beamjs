-module(beamjs_mod_require).
-export([exports/0]).
-include_lib("erlv8/include/erlv8.hrl").

exports() ->
	fun require/3.

require(Script, #erlv8_fun_invocation{} = Invocation, Filename) ->
	case application:get_env(beamjs,available_mods) of
		{ok, Mods} ->
			io:format("Mods = ~p File = ~p~n",[Mods,Filename]),
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
	{ok, NewScript} = erlv8:new_script(S),	
	beamjs:load_default_mods(NewScript),
	Self = self(),
	spawn(fun () ->
				  erlv8_script:add_handler(NewScript,erlv8_capturer,[fun (X) -> Self ! X end]),
				  erlv8_script:run(NewScript)
		  end),
	receive 
		{finished, _Result} ->
			proplists:get_value("exports",erlv8_script:global(NewScript),[]);
		_Other ->
			error(not_implemented)
	end.
	
	

