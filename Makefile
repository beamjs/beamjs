all: compile 

submodules:
	@git submodule init
	@git submodule update

deps/erlv8/ebin/erlv8.beam: submodules
	@cd deps/erlv8 && make compile

dependencies: deps/erlv8/ebin/erlv8.beam

test: compile
	@./rebar eunit

compile: dependencies
	@./rebar compile

console:
	@erl -pa ebin deps/erlv8/ebin -s erlv8 -s beamjs -eval "supervisor:start_child(beamjs_repl_sup,[\"beam.js> \", beamjs_repl_console])" -noshell