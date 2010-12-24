-module(beamjs).
-include_lib("erlv8/include/erlv8.hrl").
-export([start/0,stop/0,main/0,
		 bundles/0, set_bundles/1, modules/1, modules/2,
		 load_default_modules/1]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

bundles() ->
		case application:get_env(beamjs, bundles) of
			{ok, Bundles0} ->
				Bundles0;
			_ ->
				[default,runtime]
		end.

modules(Type,Bundle) ->
	case application:get_env(beamjs,module_bundles) of
		{ok, MB} ->
			proplists:get_value(Type, proplists:get_value(Bundle, MB, []), []);
		_ ->
			[]
	end.

modules(Type) ->
	lists:flatten(lists:map(fun (B) -> modules(Type, B) end, bundles())).

set_bundles(Bundles) ->
	application:set_env(beamjs, bundles, lists:usort([default,runtime|Bundles])).


%%%

load_default_modules(VM) ->
	Global = erlv8_vm:global(VM),
	beamjs_mod_require:init(VM),
	lists:foreach(fun({Name, Module}) ->
						  Global:set_value(Name, beamjs_mod_require:require(VM, Module))
				  end, modules(default, default)).

args(preemption) ->
	case init:get_argument(jspreemption) of
		{ok, [[V|_]]} ->
			application:set_env(erlv8, preemption_ms, list_to_integer(V));
		_ ->
			false
	end;

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

args({mod, Arg, Type}) ->
	case init:get_argument(Arg) of
		{ok, [[Alias, Mod]]} ->
			{MB, RuntimeBundle0} = 
				case application:get_env(beamjs,module_bundles) of
					{ok, MB0} ->
						{MB0, proplists:get_value(runtime,MB0,[])};
					_ ->
						application:set_env(beamjs,module_bundles, []),
						{[], []}
				end,
			TRuntimeModules0 = proplists:get_value(Type, RuntimeBundle0, []),
			TRuntimeModules = [{Alias, list_to_atom(Mod)}|TRuntimeModules0],
			RuntimeBundle = [{Type, TRuntimeModules}|proplists:delete(Type, RuntimeBundle0)],
			application:set_env(beamjs,module_bundles,[{runtime, RuntimeBundle}|proplists:delete(runtime, MB)]);
		_ ->
			false
	end;

args(mod) ->
	args({mod, mod, available}).


args(VM,bundles) ->
	case init:get_argument(bundles) of
		{ok, [Bundles]} ->
			Global = erlv8_vm:global(VM),
			AtomBundles = lists:map(fun list_to_atom/1,Bundles),
			set_bundles(AtomBundles),
			Global:set_value("module",?V8Obj([{"id","init"},{"exports", ?V8Obj([])}])),
			lists:foreach(fun(Bundle) ->
								  lists:foreach(fun({Name, Module}) ->
														Global:set_value(Name, beamjs_mod_require:require(VM, Module))
												end,  modules(default, Bundle))
						  end, AtomBundles);
		_ ->
			false
	end;

args(VM,jseval) ->
	case init:get_argument(jseval) of
		{ok, [[JS]]} ->
			erlv8_vm:run(VM, erlv8_context:get(VM), JS, {"(command line)",0,0});
		_ ->
			false
	end;

args(VM,default_mod) ->
	args({mod, default_mod, default}),
	case init:get_argument(default_mod) of
		{ok, [[Name, Module]]} ->
			Global = erlv8_vm:global(VM),
			Global:set_value(Name, beamjs_mod_require:require(VM, list_to_atom(Module)));
		_ ->
			false
	end;

args(VM, path) ->
	case init:get_argument(jspath) of
		{ok, [Paths]} ->
			lists:foreach(fun (Path) ->
								  Global = erlv8_vm:global(VM),
								  Require = Global:get_value("require"),
								  RPaths = Require:get_value("paths"),
								  RPaths:unshift(Path)
						  end, Paths);
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
	args(preemption),
	start(),
	{ok, VM} = erlv8_vm:start(),
	load_default_modules(VM),
	NoRepl = args(norepl),
	args(toolbar),
	args(mod),
	args(VM,default_mod),
	args(VM,jseval),
	args(VM,path),
	args(VM,bundles),
	args(VM,load),
	case NoRepl of
		true ->
			ok;
		false ->
			Global = erlv8_vm:global(VM),
			Global:set_value("module",?V8Obj([{"exports", beamjs_mod_repl:exports(VM)}])),
			Module = Global:get_value("module"),
			Module:set_value("id","repl",[readonly,dontdelete]),
			Require = Global:get_value("require"),
			Require:set_value("main",Module,[readonly,dontdelete]),
			erlv8_vm:run(VM, erlv8_context:get(VM), ?REPL_START, {"main",0,0})
	end,
	erlang:halt().


-include_lib("eunit/include/eunit.hrl").%
-ifdef(TEST).
suppress_kernel_logger_test() ->
	% not a test, obviously
	error_logger:delete_report_handler(error_logger_tty_h).
-endif.
