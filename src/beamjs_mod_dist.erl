-module(beamjs_mod_dist).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

	
init(_VM) ->
	ok.

exports(_VM) ->
 	?V8Obj([{"node", fun node/2},
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


node(#erlv8_fun_invocation{},[]) ->
	node().


ping(#erlv8_fun_invocation{},[Node]) when is_list(Node) ->
	case net_adm:ping(list_to_atom(Node)) of
		pong ->
			true;
		pang ->
			false
	end.

localhost(#erlv8_fun_invocation{}, []) ->
	net_adm:localhost().

nodes(#erlv8_fun_invocation{},[]) ->
	?V8Arr(nodes()).
