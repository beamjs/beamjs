-module(beamjs_repl_console).

-export([read/1,print/2]).

read(Prompt) ->
	io:get_line(Prompt).

print(VM,{compilation_failed, Error}) ->
	io:format("~s~n",[beamjs_js_formatter:format_exception(VM,Error)]); 
print(VM,{ok, Result}) ->
	io:format("~s~n",[beamjs_js_formatter:format(VM,Result)]);
print(VM,{exception, Exception}) ->
	io:format("~s~n",[beamjs_js_formatter:format_exception(VM,Exception)]).
