-module(beamjs_mod_bundles).

-behaviour(erlv8_module).

-export([exports/1, init/1]).

-include_lib("erlv8/include/erlv8.hrl").


init(_VM) ->
	ok.

exports(_VM) ->
	?V8Obj([{"loaded", fun loaded/2},
			{"unload", fun unload/2},
			{"load", fun load/2}]).


loaded(#erlv8_fun_invocation{},[]) ->
	?V8Arr(beamjs:bundles()).

unload(#erlv8_fun_invocation{},Bundles) ->
	beamjs:set_bundles(beamjs:bundles() -- lists:map(fun list_to_atom/1, Bundles)),
	?V8Arr(beamjs:bundles()).

load(#erlv8_fun_invocation{},Bundles) ->
	beamjs:set_bundles(beamjs:bundles() ++ lists:map(fun list_to_atom/1, Bundles)),
	?V8Arr(beamjs:bundles()).
