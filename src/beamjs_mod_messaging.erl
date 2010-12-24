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
																	"A constructor for `Mailbox`, an [EventEmitter](/events/EventEmitter) that emits a single message `message(Message)` for each received message.\n\n"
																	"#### Registers unnamed mailbox\n\n"
																	"    new Mailbox()\n\n"
																	"#### Registers a local named mailbox:\n\n"
																	"    new Mailbox(name)\n\n"
																	"where *name* is a string\n\n"
																	"#### Registers a global named mailbox:\n\n"
																	"    new Mailbox({global: name})\n\n"
																	"where *name* is an expression\n\n"
																	"### Example\n\n"
																	"    var mbox = new (require('messaging').Mailbox)();\n"
																	"    mbox.on(\"message\",function (msg) { ... });\n"}]))},
								 {"send", erlv8_fun:new(fun send/2,
														?V8Obj([{"__doc__",
																 "Sends a message.\n\n"
																 "    require('messaging').send(mbox,message);\n"
																 "where *message* is an expression and *mbox* is one of the following:\n\n"
																 "* A string containing a local named mailbox name\n"
																 "* An object with *global* property that contains a global named mailbox name\n"
																 "* A *pid* value (normally returned from Erlang)\n"
																 "* Mailbox object (edge case as it will be more convenient to talk to Mailbox object direcly, using methods or EventEmitter events\n"
																 "### Examples\n"
																 "#### Local named mailbox\n"
																 "    var mbox = new (require('messaging').Mailbox)('my_mbox');\n"
																 "    require('messaging').send('my_mbox', 'Hello');\n"
																 "#### Global named mailbox\n"
																 "    var mbox = new (require('messaging').Mailbox)({global: 'my_mbox'});\n"
																 "    require('messaging').send({global: 'my_mbox'}, 'Hello');</code>\n"
																 "#### Pid\n\n"
																 "    var pid = someFunction();\n"
																 "    require('messaging').send(pid, 'Hello');\n"
																 "#### Mailbox object\n"
																 "    require('messaging').send(mbox, 'Hello');\n"}]))},
								 {"__doc__", 
								  "Messaging exposes basic communication API provided by Erlang.\n\n"
								  "<code>require('messaging')</code>"}])),
	Mailbox = Obj:get_value("Mailbox"),
	Mailbox:set_value("prototype",beamjs_mod_gen_event:prototype_Manager()),
	Obj.

	
new_mailbox(#erlv8_fun_invocation{}=I,[#erlv8_fun{}=Cb]) ->
	new_mailbox(I,[noname, Cb]);

new_mailbox(#erlv8_fun_invocation{ this = This },[OptsOrName, #erlv8_fun{}=Cb]) ->

	Pid = spawn(fun () -> mailbox(This) end),
	This:set_hidden_value("mailboxServer", Pid),

	This:set_value("onMessage", Cb),

	case OptsOrName of
		noname ->
			ok;
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
			global:send(Name,erlv8_vm:untaint(Data)),
			Data;
		_ ->
			false
	end.

%% private

mailbox(This) ->
	receive Msg -> 
			OnMessage = This:get_value("onMessage"),
			This:call(OnMessage,[Msg])
	end, 
	mailbox(This).


