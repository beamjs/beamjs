-module(beamjs_repl).
-export([start/0]).

start() ->
	io:format("Beam.JS REPL~n"),
	loop().

loop() ->
	case io:fread("beam.js> ", "~s") of
		{ok, [Input]} ->
			{ok, Pid} = erlv8:new_script(Input),
			erlv8_script:register(Pid,exports,erlv8_mod_exports),
			Self = self(),
			erlv8_script:add_handler(Pid,erlv8_capturer,[fun (X) -> Self ! X end]),
			erlv8_script:run(Pid),
			receive
				{finished, Result} ->
					io:format(">>> ~p~n",[Result]);
				{exception, Exception} ->
					io:format("!!! Exception: ~p~n",[Exception]);
				_ ->
					ignore
			end;
		Other ->
			io:format("Error: ~p~n",[Other])
	end,
	loop().
