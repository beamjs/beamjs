-module(beamjs).

-export([start/0,stop/0,main/1,load_default_mods/1]).

start() ->
	application:start(beamjs).

stop() ->
	application:stop(beamjs).

load_default_mods(VM) ->
	case application:get_env(beamjs,default_mods) of
		{ok, DefaultMods} ->
			lists:foreach(fun({Name,Mod}) ->
								  erlv8_vm:register(VM,Name,Mod)
						  end, DefaultMods);
		undefined->
			skip
	end.


args(_,Resolution,[]) ->
	Resolution;
args(VM,Resolution,["-pa",Path|Rest]) ->
	code:add_patha(Path),
	args(VM,Resolution,Rest);
args(VM,Resolution,["-sname",Node|Rest]) ->
	net_kernel:start([list_to_atom(Node),shortnames]),
	args(VM,Resolution,Rest);
args(VM,Resolution,["-name",Node|Rest]) ->
	net_kernel:start([list_to_atom(Node),longnames]),
	args(VM,Resolution,Rest);
args(VM,Resolution,["-toolbar"|Rest]) ->
	toolbar:start(),
	args(VM,Resolution,Rest);
args(VM,_Resolution,["-norepl"|Rest]) ->
	args(VM,norepl,Rest);
args(VM,Resolution,["-mod",Alias,Mod|Rest]) ->
	case application:get_env(beamjs,available_mods) of
		{ok, Mods} ->
			application:set_env(beamjs,available_mods,[{Alias,list_to_atom(Mod)}|Mods]);
		undefined ->
			skip
	end,
	args(VM,Resolution,Rest);
args(VM,Resolution,["-default_mod",Alias,Mod|Rest]) ->
	erlv8_vm:register(VM,Alias,list_to_atom(Mod)),
	args(VM,Resolution,Rest);
args(VM,Resolution,[File|Rest]) when is_list(File) ->
	{ok, B} = file:read_file(File),
	S = binary_to_list(B),
	erlv8_vm:run(VM, S),
	args(VM,Resolution,Rest).
	
main(Args) ->
	case os:getenv("ERLV8_SO_PATH") of
		false ->
			os:putenv("ERLV8_SO_PATH","./deps/erlv8/priv")
	end,
	erlv8:start(),
	start(),
	{ok, VM} = erlv8_vm:new(),
	load_default_mods(VM),
	case args(VM,undefined,Args) of
		norepl ->
			ok;
		_ ->
			erlv8_vm:run(VM, "require('repl').start()")
	end.

	
