-module(beamjs_mod_dns).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
    ok.

exports(_VM) ->
    ?V8Obj([
            {"__doc__", "Node.js compatible DNS module."},
            {"resolve4", erlv8_fun:new(fun resolve4/2,
                    ?V8Obj([{"__doc__",
                                "`resolve4(name, callback)`\n\n"
                                "Resolves IPv4 address for given name\n\n"
                                "Example:\n\n"
                                "    dns.resolve('beamjs.org', function(err, ips){\n"
                                "        if (err) throw err;\n"
                                "        ips.forEach(function(ip) { console.log(ip) });\n"
                                "    })"
                            }]))
            }
        ]).

resolve4(#erlv8_fun_invocation{},[Name, #erlv8_fun{} = Callback]) when is_list(Name) ->
    case inet_res:resolve(Name, any, a) of
        {ok, Msg} ->
            { anlist, Records } = lists:keyfind(anlist, 1, inet_dns:msg(Msg)),
            Ips = lists:map(fun ip_from_record/1, Records),
            Callback:call([null, ?V8Arr(lists:map(fun ip4_to_string/1, Ips))]);
        {error, _} ->
            Callback:call([{error, "Cannot resolve"}, Name])
    end.

%
% Utility Functions
%
ip_from_record(Record) ->
    {_, Ip} = lists:keyfind(data, 1, inet_dns:rr(Record)),
    Ip.

ip4_to_string(Ip) ->
    {One, Two, Three, Four} = Ip,
    lists:concat([ One, '.', Two, '.', Three, '.', Four ]).

