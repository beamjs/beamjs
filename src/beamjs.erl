-module(beamjs).

-export([start/0,stop/0]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).
