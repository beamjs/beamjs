all: compile

submodules:
	@git submodule init
	@git submodule update

deps/erlv8/ebin/erlv8.beam: submodules
	@cd deps/erlv8 && make compile

dependencies: deps/erlv8/ebin/erlv8.beam

test: compile
	@./rebar eunit

test-commonjs: compile
	@./beamjs -norepl -jspath lib -load deps/commonjs/tests/unit-testing/1.0/program.js 

compile: dependencies
	@./rebar compile
	@cat ebin/beamjs.app | sed s/%sha%/`git log -1 --pretty=format:%h`/ > ebin/beamjs.app
