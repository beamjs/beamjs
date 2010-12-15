-module(beamjs_repl_console).

-export([read/1,print/2]).

read(Prompt) ->
	io:get_line(Prompt).

print(Script,{compilation_failed, Error}) ->
	io:format("~s~n",[beamjs_js_formatter:format_exception(Script,Error)]); 
print(Script,{finished, Result}) ->
	io:format("~s~n",[beamjs_js_formatter:format(Script,Result)]);
print(Script,{exception, Exception}) ->
	io:format("~s~n",[beamjs_js_formatter:format_exception(Script,Exception)]).
