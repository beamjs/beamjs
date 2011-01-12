-module(beamjs).
-include_lib("erlv8/include/erlv8.hrl").
-export([start/0,stop/0,main/0]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

%%%

install_require(VM) ->
	Global = erlv8_vm:global(VM),
	beamjs_mod_require:init(VM),
	Global:set_value("require", beamjs_mod_require:exports(VM)).

%%%

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
	end.

args(VM,bundles) ->
	case init:get_argument(bundles) of
		{ok, [Bundles]} ->
			lists:foreach(fun(Bundle) ->
								  case (catch beamjs_bundle:load(VM, Bundle)) of
									  {'EXIT', {{bundle, {throw, {error, #erlv8_object{}=E}}}, _}} ->
										  io:format("~s~n",[beamjs_js_formatter:format_exception(VM,E)]);
									  _ ->
										  ignore
								  end
						  end, Bundles);
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
	install_require(VM),
	args(VM,jseval),
	beamjs_bundle:load(VM, default),
	NoRepl = args(norepl),
	args(toolbar),
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
			erlv8_vm:run(VM, erlv8_context:get(VM), ?REPL_START, {"main",0,0}),
			receive 
				_ ->
					ok
			end
	end,
	erlang:halt().


-include_lib("eunit/include/eunit.hrl").%
-ifdef(TEST).
suppress_kernel_logger_test() ->
	% not a test, obviously
	error_logger:delete_report_handler(error_logger_tty_h).
-endif.
