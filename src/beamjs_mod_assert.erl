-module(beamjs_mod_assert).
-export([exports/1,init/1]).

-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(VM) ->
	Constructor = erlv8_vm:taint(VM,fun new_AssertionError/2),
	erlv8_vm:stor(VM,{?MODULE, 'AssertionError'}, Constructor),
	?V8Obj([{"AssertionError", Constructor},
			{"equal", fun assert_equal/2}]).

new_AssertionError(#erlv8_fun_invocation{ this = This }=I, [#erlv8_object{}=Options]) ->
	case I:is_construct_call() of
		false ->
			undefined;
		true ->
			This:set_value("message",Options:get_value("message")),
			This:set_value("actual",Options:get_value("actual")),
			This:set_value("expected",Options:get_value("expected"))
	end.

assert_equal(#erlv8_fun_invocation{ vm = VM }, [Actual, Expected, Message]) ->
	Equals = 
		case Actual of
			#erlv8_object{} ->
				Actual:equals(Expected);
			#erlv8_array{} ->
				Actual:equals(Expected);
			#erlv8_fun{} ->
				Actual:equals(Expected);
			_ ->
				Actual == Expected
		end,
	case Equals of
		true ->
			ok;
		false ->
			AE = erlv8_vm:retr(VM,{?MODULE,'AssertionError'}),
			Exception = AE:instantiate([?V8Obj([{"actual", Actual}, 
												{"expected", Expected},
												{"message", Message}])]),
			{throw, Exception}
	end;

assert_equal(#erlv8_fun_invocation{}=I, [Actual, Expected]) ->
	assert_equal(I,[Actual, Expected,""]).

