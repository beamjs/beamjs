-module(beamjs_repl_console).

-export([read/1,print/1]).

read(Prompt) ->
	{ok, [Input]} = io:fread(Prompt,"~s"),
	Input.

print(S) when is_list(S) ->
	io:format("~s~n",[S]);
print({finished, Result}) ->
	io:format(">>> ~p~n",[Result]);
print({exception, Exception}) ->
	io:format("    Exception: ~p~n",[Exception]).
