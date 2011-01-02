-module(beamjs_mod_dist).
-export([exports/1,init/1]).
-behaviour(beamjs_module).
-include_lib("erlv8/include/erlv8.hrl").

	
init(_VM) ->
	ok.

exports(VM) ->
	Node = erlv8_vm:taint(VM, fun new_node/2),
	Node:set_value("prototype", prototype_Node()),
	erlv8_vm:stor(VM,{?MODULE, ctor_Node}, Node),
 	?V8Obj([{"node", fun node/2},
			{"Node", Node},
			{"ping", erlv8_fun:new(fun ping/2, 
								   ?V8Obj([{"__doc__", 
											"<code>ping(nodename)</code>\n\n"
											"Tries to set up a connection to *nodename*.\n\n"
											"Returns `false` if it fails, or `true` if it is successful."}]))},
			{"localhost", erlv8_fun:new(fun localhost/2,
										?V8Obj([{"__doc__",
												 "<code>localhost()</code>\n\n"
												 "Returns the name of the localhost. If *beamjs* was started with "
												 "the -name command line flag, it will return a fully qualified name."}]))},
			{"nodes", fun nodes/2}]).

prototype_Node() ->
	?V8Obj([{"ping", fun ping/2}]).

node(#erlv8_fun_invocation{},[]) ->
	node().

new_node(#erlv8_fun_invocation{ this = This },[Name]) when is_list(Name) ->
	This:set_value("name", Name).


ping(#erlv8_fun_invocation{ this = This },[]) ->
	Node = list_to_atom(This:get_value("name")),
	case net_adm:ping(Node) of
		pong ->
			true;
		pang ->
			false
	end.

localhost(#erlv8_fun_invocation{}, []) ->
	net_adm:localhost().

nodes(#erlv8_fun_invocation{ vm = VM },[]) ->
	NodeCtor = erlv8_vm:retr(VM, {?MODULE, ctor_Node}),
	?V8Arr(lists:map(fun (Node) ->
							 NodeObj = NodeCtor:instantiate([atom_to_list(Node)]),
							 NodeObj:set_prototype(prototype_Node()),
							 NodeObj
					 end, nodes())).
