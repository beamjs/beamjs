-module(beamjs_mod_gen_event).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

% internal
-export([prototype_Manager/0]).

%-behaviour(gen_event). % commented this out just because of init/1 conflict warning
-export([handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(PID, This:get_hidden_value("eventManager")).

init({gen_event, Handler}) -> %% gen_event
	{ok, Handler};

init({{gen_event, Handler}, Term}) -> %% gen_event
	case Handler:get_value("onTakeover") of
		#erlv8_fun{}=F ->
			Handler:call(F, [Term]);
		_ ->
			skip
	end,
	{ok, Handler};

init(_VM) -> %% erlv8_module
	ok.

prototype_Manager() ->
	?V8Obj([
			{"addHandler", fun add_handler/2},
			{"deleteHandler", fun delete_handler/2},
			{"swapHandler", fun swap_handler/2},
			{"whichHandlers", fun which_handlers/2},
			{"notify", fun notify/2}
		   ]).

prototype_Handler() ->
	?V8Obj([
			{"removeHandler", fun set_remove_handler/2}
		   ]).


exports(VM) ->
	Manager = erlv8_vm:taint(VM,fun new_gen_event/2),
	Manager:set_value("prototype",prototype_Manager()),
	Handler = erlv8_vm:taint(VM,fun new_handler/2),
	Handler:set_value("prototype",prototype_Handler()),
	?V8Obj([{"Manager",Manager},
			{"Handler",Handler}]).
			

new_gen_event(#erlv8_fun_invocation{ this = This },[]) ->
	{ok, Pid} = gen_event:start(), 
	This:set_hidden_value("eventManager", Pid),
	undefined.

new_handler(#erlv8_fun_invocation{},[]) ->
	undefined;
new_handler(#erlv8_fun_invocation{ this = This } = I,[#erlv8_object{}=Obj]) ->
	lists:foreach(fun (K, V) ->
						  This:set_value(K,V)
				  end, Obj:proplist()),
	new_handler(I,[]);
new_handler(#erlv8_fun_invocation{ this = This } = I,[#erlv8_fun{}=F]) ->
	This:set_value("onEvent",F),
	new_handler(I,[]).
	

add_handler(#erlv8_fun_invocation{ this = This },[#erlv8_object{}=Handler]) ->
	Pid = ?PID,
	gen_event:add_handler(Pid, {?MODULE, Handler}, {gen_event, Handler}).

delete_handler(#erlv8_fun_invocation{ this = This },[#erlv8_object{}=Handler|Args]) ->
	Pid = ?PID,
	gen_event:delete_handler(Pid, {?MODULE, Handler}, Args).

swap_handler(#erlv8_fun_invocation{ this = This },[#erlv8_object{}=Handler1,#erlv8_object{}=Handler2]) ->
	Pid = ?PID,
	gen_event:swap_handler(Pid, {{?MODULE, Handler1}, [swap_handler]}, {{?MODULE, Handler2}, {gen_event,Handler2}}).

which_handlers(#erlv8_fun_invocation{ this = This },[]) ->
	Pid = ?PID,
	?V8Arr(lists:map(fun ({_, Id}) -> Id end, gen_event:which_handlers(Pid))).

notify(#erlv8_fun_invocation{ this = This },[Event]) ->
	Pid = ?PID,
	gen_event:notify(Pid, Event).

set_remove_handler(#erlv8_fun_invocation{ this = This },[]) ->
	This:set_hidden_value("remove_handler", true).


%% gen_event
handle_event(Event, #erlv8_object{} = Handler) ->
	case Handler:get_value("onEvent") of
		#erlv8_fun{}=F ->
			Handler:call(F,[Event]),
			RemoveHandler = Handler:get_hidden_value("remove_handler"),
			Handler:set_hidden_value("remove_handler", undefined),
			case RemoveHandler of
				true ->
					remove_handler;
				_ ->
					{ok, Handler}
			end;
		_ ->
			{ok, Handler}
	end.


handle_call(_Req,State) ->
	{ok, ok, State}.

handle_info(_Info,State) ->
	{ok, State}.

terminate(Args, Handler) ->
	case Handler:get_value("onTerminate") of
		#erlv8_fun{}=F ->
			Handler:call(F, Args);
		_ ->
			undefined
	end.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



