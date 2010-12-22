-module(beamjs).
-include_lib("erlv8/include/erlv8.hrl").
-export([start/0,stop/0,main/0,load_default_mods/1]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

load_default_mods(VM) ->
	case application:get_env(beamjs,default_mods) of
		{ok, DefaultMods} ->
			lists:foreach(fun({Name,Mod}) ->
								  erlv8_vm:register(VM,Name,Mod)
						  end, DefaultMods);
		undefined->
			skip
	end.

args(norepl) ->
	case init:get_argument(norepl) of
		{ok, _} ->
			true;
		_ ->
			false
	end;
args(toolbar) ->
	case init:get_argument(toolbar) of
		{ok, _} ->
			toolbar:start();
		_ ->
			false
	end;

args(mod) ->
	case init:get_argument(mod) of
		{ok, [[Alias, Mod]]} ->
			case application:get_env(beamjs,available_mods) of
				{ok, Mods} ->
					application:set_env(beamjs,available_mods,[{Alias,list_to_atom(Mod)}|Mods]);
				undefined ->
					skip
			end;
		_ ->
			false
	end.

args(VM,jseval) ->
	case init:get_argument(jseval) of
		{ok, [[JS]]} ->
			erlv8_vm:run(VM, erlv8_context:get(VM), JS, {"(command line)",0,0});
		_ ->
			false
	end;

args(VM,default_mod) ->
	case init:get_argument(default_mod) of
		{ok, [[Alias, Mod]]} ->
			erlv8_vm:register(VM,Alias,list_to_atom(Mod));
		_ ->
			false
	end;

args(VM,load) ->
	case init:get_argument(load) of
		{ok, [Files]} ->
			lists:foreach(fun (File) ->
								  Global = erlv8_vm:global(VM),
								  Require = Global:get_value("require"),
								  Global:set_value("module",?V8Obj([])),
								  Module = Global:get_value("module"),
								  Module:set_value("id",File,[dontdelete,readonly]),
								  Require:set_value("main",Module),
								  case Require:call([File]) of
									  {throw, {error, #erlv8_object{}=E}} ->
										  io:format("~s~n",[beamjs_js_formatter:format_exception(VM,E)]);
									  _ ->
										  ignore
								  end
						  end, Files);
		_ ->
			false
	end.

	
-define(REPL_START,"require('repl').start()").

main() ->
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			os:putenv("ERLV8_SO_PATH","./deps/erlv8/priv")
	end,
	erlv8:start(),
	start(),
	{ok, VM} = erlv8_vm:start(),
	load_default_mods(VM),
	NoRepl = args(norepl),
	args(toolbar),
	args(mod),
	args(VM,default_mod),
	args(VM,jseval),
	args(VM,load),
	case NoRepl of
		true ->
			ok;
		false ->
			erlv8_vm:run(VM, erlv8_context:get(VM), ?REPL_START, {"main",0,0})
	end,
	erlang:halt().


-include_lib("eunit/include/eunit.hrl").%
-ifdef(TEST).
suppress_kernel_logger_test() ->
	% not a test, obviously
	error_logger:delete_report_handler(error_logger_tty_h).
-endif.
