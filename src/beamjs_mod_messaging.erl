-module(beamjs_mod_messaging).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init({gen_server2,This, Emitter}) ->
	{ok, {This, Emitter}};
init(_VM) ->
	ok.

exports(VM) ->
 	Obj = erlv8_vm:taint(VM,
						 ?V8Obj([{"Mailbox", erlv8_fun:new(fun new_mailbox/2,
														   ?V8Obj([{"__doc__",
																	"A constructor for Mailbox, an [EventEmitter](/events/EventEmitter) that emits a single message *message(Message)** for each received message.\n\n"
																	"#### Registers unnamed mailbox\n\n"
																	"<code>new Mailbox()</code>\n\n"
																	"#### Registers a local named mailbox:\n\n"
																	"<code>new Mailbox(name)</code>\n\n"
																	"where *name* is a string\n\n"
																	"#### Registers a global named mailbox:\n\n"
																	"<code>new Mailbox({global: name})</code>\n\n"
																	"where *name* is an expression\n\n"
																	"### Example\n\n"
																	"<code>var mbox = new (require('messaging').Mailbox)();\n\n"
																	"mbox.on(\"message\",function (msg) { ...});</code>\n\n"}]))},
								 {"send", erlv8_fun:new(fun send/2,
														?V8Obj([{"__doc__",
																 "Sends a message.\n\n"
																 "<code>require('messaging').send(mbox,message);</code>\n\n"
																 "where *message* is an expression and *mbox* is one of the following:\n\n"
																 "* A string containing a local named mailbox name\n\n"
																 "* An object with *global* property that contains a global named mailbox name\n\n"
																 "* A *pid* value (normally returned from Erlang)\n\n"
																 "* Mailbox object (edge case as it will be more convenient to talk to Mailbox object direcly, using methods or EventEmitter events\n\n"
																 "### Examples\n\n"
																 "#### Local named mailbox\n\n"
																 "<code>var mbox = new (require('messaging').Mailbox)('my_mbox');\n\n"
																 "require('messaging').send('my_mbox', 'Hello');</code>\n\n"
																 "#### Global named mailbox\n\n"
																 "<code>var mbox = new (require('messaging').Mailbox)({global: 'my_mbox'});\n\n"
																 "require('messaging').send({global: 'my_mbox'}, 'Hello');</code>\n\n"
																 "#### Pid\n\n"
																 "<code>var pid = someFunction();\n\n"
																 "require('messaging').send(pid, 'Hello');</code>\n\n"
																 "#### Mailbox object\n\n"
																 "<code>require('messaging').send(mbox, 'Hello');</code>\n\n"}]))},
								 {"__doc__", 
								  "Messaging exposes basic communication API provided by Erlang.\n\n"
								  "<code>require('messaging')</code>"}])),
	Mailbox = Obj:get_value("Mailbox"),
	Mailbox:set_prototype(beamjs_mod_events:prototype_EventEmitter()),
	Obj.

	
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

send(#erlv8_fun_invocation{},[Pid, Data]) when is_pid(Pid) ->
	Pid ! Data;

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



