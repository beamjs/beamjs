-module(beamjs_mod_events).

-behaviour(erlv8_module).

-export([exports/1, init/1]).

% for internal purposes
-export([prototype_EventEmitter/0]).


%-behaviour(gen_event). % commented this out just because of init/1 conflict warning
-export([handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).


-include_lib("erlv8/include/erlv8.hrl").

init({gen_event, Type, This, Event, Listener}) -> %% gen_event
	{ok, {Type, This, Event, Listener}};

init(_VM) -> %% erlv8_module
	ok.

	   
exports(VM) ->
	EventEmitter = erlv8_vm:taint(VM,erlv8_fun:new(fun new_event_emitter/2,
												   ?V8Obj([{"__doc__",
															"Node.js-like EventEmitter API for gen_event. Read more on EventEmitter at [node.js documentation](http://nodejs.org/docs/v0.3.2/api/events.html)"}]))),
	EventEmitter:set_value("prototype",prototype_EventEmitter()),
	?V8Obj([{"EventEmitter", EventEmitter}]).

												   

prototype_EventEmitter() ->
	?V8Obj([{"emit", fun emit/2},
			{"addListener", fun add_listener/2},
			{"on", fun add_listener/2},
			{"once", fun once/2},
			{"listeners", fun listeners/2},
			{"removeListener", fun remove_listener/2},
			{"removeAllListeners", fun remove_all_listeners/2}]).

new_event_emitter(#erlv8_fun_invocation{ this = This },[]) ->
	{ok, Pid} = gen_event:start(), %% not sure if we want start or start_link here
	This:set_hidden_value("eventManager", Pid),
	This:set_hidden_value("_listeners",?V8Obj([])),
	undefined.

emit(#erlv8_fun_invocation{ this = This } = Invocation,[Event|Args]) ->
	Pid = This:get_hidden_value("eventManager"),
	Listeners = This:get_hidden_value("_listeners"),
	EventListeners = Listeners:get_value(Event),
	Handlers = gen_event:which_handlers(Pid),
	case EventListeners:length() /= length(Handlers) of
		true -> %% take care of additions (removals will be taken care by a handler itself
			lists:foreach(fun (L) ->
								  case lists:filter(fun ({?MODULE, {_, HandlerEvent, Listener}}) ->
															HandlerEvent == Event andalso Listener:equals(L);
														(_) ->
															false
													end, Handlers) of
									  [] -> %% no matching handler 
										  add_listener(Invocation, [Event, L]);
									  _ ->
										  ok
								  end
						  end, EventListeners:list());
		false ->
			ignore
	end,
	gen_event:notify(Pid,{event, Event, Args}),
	undefined.

add_listener(Type, #erlv8_fun_invocation{ this = This },[Event, Listener]) ->
	Pid = This:get_hidden_value("eventManager"),
	Ref = make_ref(),
	gen_event:notify(Pid,{event, "newListener", [Event, Listener]}),
	gen_event:add_handler(Pid, {?MODULE, {Ref, Event, Listener}}, {gen_event, Type, This, Event, Listener}),
	Listeners = This:get_hidden_value("_listeners"),
	EventListeners = Listeners:get_value(Event),
	case EventListeners of
		undefined ->
			Listeners:set_value(Event, ?V8Arr([Listener]));
		_ ->
			EventListeners:push(Listener)
	end,
	ok.

add_listener(#erlv8_fun_invocation{}=I,Args) ->
	add_listener(normal, I, Args).

once(#erlv8_fun_invocation{}=I,Args) ->
	add_listener(once, I, Args).

listeners(#erlv8_fun_invocation{}, []) ->
	{throw, {error, "Event name should be specified"}};

listeners(#erlv8_fun_invocation{ this = This}, [Event]) ->
	Listeners = This:get_hidden_value("_listeners"),
	Listeners:get_value(Event).

remove_listener(#erlv8_fun_invocation{ this = This}, [Listener]) -> 
	Pid = This:get_hidden_value("eventManager"),
	Listeners = This:get_hidden_value("_listeners"),
	lists:foreach(fun ({_,EventListeners}) ->
						  lists:foldl(fun (L,I) ->
											  case L:equals(Listener) of
												  true ->
													  EventListeners:delete(I), 
													  I;
												  false ->
													  I+1
											  end
									  end, 0, EventListeners:list())
				  end, Listeners:proplist()),
	gen_event:notify(Pid,{remove_listener, Listener}),
	undefined.	

remove_all_listeners(#erlv8_fun_invocation{  this = This },[Event]) ->
	Pid = This:get_hidden_value("eventManager"),
	Listeners = This:get_hidden_value("_listeners"),
	Listeners:set_value(Event,?V8Arr([])),
	gen_event:notify(Pid,{remove_all, Event}),
	undefined.


%% gen_event
handle_event({event, Event, Args}, {normal, _This, Event, _Listener}=State) ->
	call_if_present(Args, State);

handle_event({event, Event, Args}, {once, _This, Event, _Listener}=State) ->
	call_if_present(Args, State),
	remove_handler; % remove unconditionally

handle_event({remove_all, Event}, {_, _This, Event, _}) ->
	remove_handler;

handle_event({remove_listener, ListenerFun}, {_, _, _, Listener}=State) ->
	case ListenerFun:equals(Listener) of
		true ->
			remove_handler;
		false ->
			{ok, State}
	end;

handle_event(_, State) ->
	{ok, State}.

handle_call(_Req,State) ->
	{ok, ok, State}.

handle_info(_Info,State) ->
	{ok, State}.

terminate(remove_handler, {_, This, Event, Listener}=_State) ->
	Listeners = This:get_hidden_value("_listeners"),
	EventListeners = Listeners:get_value(Event),
	lists:foldl(fun (L,I) ->
						case L:equals(Listener) of
							true ->
								EventListeners:delete(I), 
								I;
							false ->
								I+1
						end
				end, 0, EventListeners:list()),
	ok;

terminate(_Args, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% private
call_if_present(Args, {_, This, Event, Listener}=State) ->
	Listeners = This:get_hidden_value("_listeners"),
	EventListeners = Listeners:get_value(Event),
	case lists:filter(fun (L) ->  L:equals(Listener) end, EventListeners:list()) of
		[] ->
			remove_handler;
		_ ->
			Listener:call(Args),
			{ok, State}
	end.


%%% TESTS
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
one_listener_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	erlv8_vm:run(VM,
				 "var emitter = new (require('events').EventEmitter)();"
				 "var msg = require('messaging');"
				 "emitter.addListener('event', function(x) { msg.send(reportTo, true); });"
				 "emitter.emit('event',1);"),
	receive 
		true ->
			ok;
		Other ->
			error({bad_result, Other})
	end,
	beamjs:stop(), erlv8:stop().

once_listener_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	erlv8_vm:run(VM,
				 "var emitter = new (require('events').EventEmitter)();"
				 "var msg = require('messaging');"
				 "var root = { i: 0};"
				 "emitter.once('event', function() { root.i++; msg.send(reportTo, root.i); });"
				 "emitter.emit('event');"
				 "emitter.emit('event');"),
	once_listener_test_loop(false),
	beamjs:stop(), erlv8:stop().

once_listener_test_loop(F) ->
	receive 
		1 ->
			once_listener_test_loop(true);
		2 ->
			error({bad_result, 2});
		Other ->
			error({bad_result, Other})
	after 200 ->
			?assert(F)
	end.

two_listeners_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	erlv8_vm:run(VM,
				 "var emitter = new (require('events').EventEmitter)();"
				 "var msg = require('messaging');"
				 "var root = { i: 0};"
				 "emitter.on('event', function() { root.i++; msg.send(reportTo, root.i); });"
				 "emitter.on('event', function() { root.i++; msg.send(reportTo, root.i); });"
				 "emitter.emit('event',1);"),
	two_listeners_test_loop(0),
	beamjs:stop(), erlv8:stop().

two_listeners_test_loop(3) ->
	error({bad_result,3});
two_listeners_test_loop(N) ->
	receive 
		M when is_integer(M) ->
			two_listeners_test_loop(M);
		Other ->
			error({bad_result, Other})
	after 200 ->
			?assertEqual(2,N)
	end.

new_listener_event_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	erlv8_vm:run(VM,
				 "var emitter = new (require('events').EventEmitter)();"
				 "var msg = require('messaging');"
				 "var listener = function () {};"
				 "emitter.addListener('newListener', function(e,l) {  msg.send(reportTo, {rightListener: (l==listener), event: e});});"
				 "emitter.addListener('event', listener);"),
	receive 
		{erlv8_object, _, _} = Object -> 
			?assertEqual(true,Object:get_value("rightListener")),
			?assertEqual("event",Object:get_value("event"))
	end,
	beamjs:stop(), erlv8:stop().

listener_removal_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
	{ok, Arr} = erlv8_vm:run(VM,
							 "var emitter = new (require('events').EventEmitter)();"
							 "var root = {};"
							 "var l = function() { root.value = true };"
							 "emitter.addListener('event', l);"
							 "emitter.removeListener(l);"
							 "emitter.emit('event',1);"
							 "emitter.listeners('event')"),
	?assertEqual(0,Arr:length()),
    Global = erlv8_vm:global(VM),
    Root = Global:get_value("root"),
	?assertEqual(undefined,Root:get_value("value")),
	beamjs:stop(), erlv8:stop().

event_listeners_removal_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
	{ok, Arr} = erlv8_vm:run(VM,
							 "var emitter = new (require('events').EventEmitter)();"
							 "var root = {};"
							 "var l = function() { root.value = true };"
							 "emitter.addListener('event', l);"
							 "emitter.removeAllListeners('event');"
							 "emitter.emit('event',1);"
							 "emitter.listeners('event')"),
	?assertEqual(0,Arr:length()),
    Global = erlv8_vm:global(VM),
    Root = Global:get_value("root"),
	?assertEqual(undefined,Root:get_value("value")),
	beamjs:stop(), erlv8:stop().

listeners_manual_removal_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	{ok, Listeners} = erlv8_vm:run(VM,
								   "var emitter = new (require('events').EventEmitter)();"
								   "var msg = require('messaging');"
								   "var l1 = function() { msg.send(reportTo, 1) };"
								   "var l2 = function() { msg.send(reportTo, 2) };"
								   "emitter.addListener('event', l1);"
								   "emitter.addListener('event', l2);"
								   "emitter.listeners('event');"),
    Global = erlv8_vm:global(VM),
    L1 = Global:get_value("l1"),
    L2 = Global:get_value("l2"),
	?assertEqual(2,Listeners:length()),
	ListenersList = Listeners:list(),
	?assert(L1:equals(lists:nth(1,ListenersList))),
	?assert(L2:equals(lists:nth(2,ListenersList))),
	Listeners:delete(0),
	erlv8_vm:run(VM,"emitter.emit('event');"),
	listeners_manual_removal_test_loop(false),
	beamjs:stop(), erlv8:stop().

listeners_manual_removal_test_loop(F) ->
	receive 
		2 ->
			listeners_manual_removal_test_loop(true);
		1 ->
			error({bad_result, 1});
		Other ->
			error({bad_result, Other})
	after 200 ->
			?assert(F)
	end.

listeners_manual_addition_test() ->
	erlv8:start(), beamjs:start(),
	{ok, VM} = erlv8_vm:start(),
	beamjs:set_bundles([node_compat,erlang]),
	beamjs:load_default_mods(VM),
    Global = erlv8_vm:global(VM),
	Global:set_value("reportTo",self()),
	{ok, Listeners} = erlv8_vm:run(VM,
								   "var emitter = new (require('events').EventEmitter)();"
								   "var msg = require('messaging');"
								   "var l1 = function() { msg.send(reportTo, 1) };"
								   "var l2 = function() { msg.send(reportTo, 2) };"
								   "emitter.addListener('event', l1);"
								   "emitter.listeners('event');"),
    Global = erlv8_vm:global(VM),
    L1 = Global:get_value("l1"),
    L2 = Global:get_value("l2"),
	?assertEqual(1,Listeners:length()),
	ListenersList = Listeners:list(),
	?assert(L1:equals(lists:nth(1,ListenersList))),
	Listeners:push(L2),
	erlv8_vm:run(VM,"emitter.emit('event');"),
	listeners_manual_addition_test_loop(0),
	beamjs:stop(), erlv8:stop().

listeners_manual_addition_test_loop(N) ->
	receive 
		M when is_integer(M) ->
			listeners_manual_addition_test_loop(N+M);
		Other ->
			error({bad_result, Other})
	after 200 ->
			?assertEqual(3,N)
	end.


-endif.

	
