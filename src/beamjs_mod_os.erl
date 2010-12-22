-module(beamjs_mod_os).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

%% Operating System Specific Functions

init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([
			{"getenv", erlv8_fun:new(fun getenv/2, ?V8Obj(["__doc__",
						"<code>getenv(varname)</code> -> String\n\n"
						"Returns the Value of the environment variable VarName.\n\n"
						"Returns `false` if the environment variable is undefined."]))},
			{"putenv", erlv8_fun:new(fun putenv/2, ?V8Obj(["__doc__",
						"<code>putenv(varname, value)</code> -> true\n\n"
						"Sets a new Value for the environment variable VarName."]))},
			{"type", erlv8_fun:new(fun type/2, ?V8Obj(["__doc__",
						"<code>type()</code> -> [unix, linux]\n\n"
						"Returns the Osfamily and, in some cases, Osname of the current operating system."]))},
			{"version", erlv8_fun:new(fun version/2, ?V8Obj(["__doc__",
						"<code>version()</code> -> [2,6,37] \n\n"
						"Returns the Osfamily and, in some cases, Osname of the current operating system."]))}
	]).

getenv(#erlv8_fun_invocation{},[VarName]) when is_list(VarName) ->
	os:getenv(VarName).

putenv(#erlv8_fun_invocation{},[VarName, Value]) when is_list(VarName) ->
	os:putenv(VarName, Value).

type(#erlv8_fun_invocation{},[]) ->
	{Osfamily, Osname} = os:type(),
	?V8Arr([Osfamily, Osname]).

version(#erlv8_fun_invocation{},[]) ->
	{Major, Minor, Release} = os:version(),
	?V8Arr([Major, Minor, Release]).
