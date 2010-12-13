-module(beamjs_repl_console).

-export([read/1,print/1]).

read(Prompt) ->
	io:get_line(Prompt).

print({finished, Result}) ->
	io:format(">>> ~p~n",[Result]);
print({exception, Exception}) ->
	io:format("    Exception: ~p~n",[Exception]).
