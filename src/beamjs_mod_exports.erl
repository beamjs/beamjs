-module(beamjs_mod_exports).
-export([exports/0,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.


exports() ->
	[].
