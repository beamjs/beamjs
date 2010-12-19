-module(beamjs_mod_messaging).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").
%-behaviour(gen_server2).
%% gen_server2 callbacks
-export([handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).


init({gen_server2,This, Emitter}) ->
	{ok, {This, Emitter}};
init(_VM) ->
	ok.

exports(_VM) ->
 	?V8Obj([{"Mailbox", fun new_mailbox/2},
			{"Node", ?V8Obj([{this, node()},
							 {ping, fun nodes_ping/2},
							 {nodes, fun nodes_nodes/2}])}]).
			

prototype() ->
	?V8Obj([{"send", fun send/2}]).


new_mailbox(#erlv8_fun_invocation{}=I,[]) ->
	new_mailbox(I,[noname]);

new_mailbox(#erlv8_fun_invocation{ this = This }=I,[OptsOrName]) ->
	Global = I:global(),
	Require = Global:get_value("require"),
	EventsMod = Require:call(["events"]),
	EventEmitterCtor = EventsMod:get_value("EventEmitter"),

	EventEmitterCtor:call(This,[]),

	This:set_prototype(prototype()),
	Prototype = This:get_prototype(),
	Prototype:set_prototype(beamjs_mod_events:prototype_EventEmitter()), %% FIXME?
	Emitter = This:get_value("emit"),

	{ok, Pid} =
	case OptsOrName of
		noname ->
			gen_server2:start(?MODULE, {gen_server2, This, Emitter}, []);
		Name when is_list(Name) ->
			gen_server2:start({local,list_to_atom(Name)}, ?MODULE, {gen_server2, This, Emitter}, []);
		{erlv8_object,_,_} ->
			case  OptsOrName:get_value("global") of
				undefined ->
					{throw, {error, "new Mailbox() accepts either zero arguments or a string, or {global: string}"}};
				GlobalName ->
					gen_server2:start({global,GlobalName}, ?MODULE, {gen_server2, This, Emitter}, [])
			end
	end,
	This:set_hidden_value("mailboxServer", Pid),
	undefined.

send(#erlv8_fun_invocation{},[Name, Data]) when is_list(Name) ->
	list_to_existing_atom(Name) ! Data;

send(#erlv8_fun_invocation{},[{erlv8_object, _, _}=O,Data])  ->
	case O:get_value("global") of
		undefined ->
			Pid = O:get_hidden_value("mailboxServer"),
			Pid ! Data;
		Name when not is_tuple(Name) ->
			global:sync(), %% FIXME: remove this when global API will be exposed
			global:send(Name,untaint(Data)),
			Data;
		_ ->
			false
	end.


nodes_ping(#erlv8_fun_invocation{},[Node]) ->
	case net_adm:ping(list_to_atom(Node)) of
		pong ->
			true;
		pang ->
			false
	end.

nodes_nodes(#erlv8_fun_invocation{},[]) ->
	nodes().
	

% gen_server2 

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(Info, {This,Emitter}=State) ->
	Emitter:call(This,["info",Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% private

untaint({erlv8_object, _}=O) ->
	{erlv8_object,lists:map(fun ({Key, Val}) ->
									{Key, untaint(Val)}
							end,O:proplist())};
untaint({erlv8_fun, _}=F) -> %% broken
	{erlv8_object,untaint(F:object())};
untaint([H|T]) ->
	[untaint(H)|untaint(T)];
untaint([]) ->
	[];
untaint(Other) ->
	Other.



