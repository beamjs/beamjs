-module(beamjs_mod_messaging).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init({gen_server2,This, Emitter}) ->
	{ok, {This, Emitter}};
init(_VM) ->
	ok.

exports(_VM) ->
 	?V8Obj([{"Mailbox", fun new_mailbox/2},
			{"send", fun send/2}]).
	
new_mailbox(#erlv8_fun_invocation{}=I,[]) ->
	new_mailbox(I,[noname]);

new_mailbox(#erlv8_fun_invocation{ this = This }=I,[OptsOrName]) ->
	Global = I:global(),
	Require = Global:get_value("require"),
	EventsMod = Require:call(["events"]),
	EventEmitterCtor = EventsMod:get_value("EventEmitter"),

	EventEmitterCtor:call(This,[]),

	Prototype = This:get_prototype(),
	Prototype:set_prototype(beamjs_mod_events:prototype_EventEmitter()), %% FIXME?
	Emitter = This:get_value("emit"),

	Pid = spawn(fun () -> mailbox(This, Emitter) end),
	This:set_hidden_value("mailboxServer", Pid),

	case OptsOrName of
		Name when is_list(Name) ->
			register(list_to_atom(Name), Pid), ok;
		{erlv8_object,_,_} ->
			case  OptsOrName:get_value("global") of
				undefined ->
					{throw, {error, "new Mailbox() accepts either zero arguments or a string, or {global: string}"}};
				GlobalName ->
					case global:register_name(GlobalName, Pid) of
						yes ->
							true;
						no ->
							{throw, {error, "Wasn't able to register a global name " ++ GlobalName }}
					end
			end
	end.

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

%% private

mailbox(This, Emitter) ->
	receive Msg -> This:call(Emitter, ["message",Msg]) end, 
	mailbox(This, Emitter).


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



