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
	@./beamjs -norepl -bundles node_compat commonjs  -load deps/commonjs/tests/unit-testing/1.0/program.js 

compile: dependencies
	@./rebar compile
	@cat ebin/beamjs.app | sed s/%sha%/`git log -1 --pretty=format:%h`/ > ebin/beamjs.app

release: compile
	@rm -rf rel/beamjs
	@rm -rf rel/apps
	@mkdir -p rel/apps/beamjs
	@cp -R ebin rel/apps/beamjs
	@cp -R deps/erlv8 rel/apps/
	@./rebar generate
	@rm -rf rel/apps

install: release
	rm -rf /usr/local/lib/beamjs
	cp -R rel/beamjs /usr/local/lib
	ln -s /usr/local/lib/beamjs/bin/beamjs /usr/local/bin
	@rm -rf rel/beamjs

archive: release
	@tar c -C rel beamjs > beamjs.tar
	@gzip beamjs.tar 
	@rm -rf rel/beamjs
