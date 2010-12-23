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
			{"equal", fun assert_equal/2},
			{"strictEqual", fun assert_strictEqual/2},
			{"notEqual", fun assert_notEqual/2},
			{"notStrictEqual", fun assert_notStrictEqual/2},
			{"deepEqual", fun assert_strictEqual/2},
			{"ok", fun assert_ok/2},
			{"throws", fun assert_throws/2}]).

new_AssertionError(#erlv8_fun_invocation{ this = This }=I, [#erlv8_object{}=Options]) ->
	case I:is_construct_call() of
		false ->
			undefined;
		true ->
			This:set_value("message",Options:get_value("message")),
			This:set_value("actual",Options:get_value("actual")),
			This:set_value("expected",Options:get_value("expected"))
	end;

new_AssertionError(#erlv8_fun_invocation{ vm = VM }=I, [Message]) when is_list(Message) ->
	new_AssertionError(I, [erlv8_vm:taint(VM,?V8Obj([{"message", Message}]))]);
	
new_AssertionError(#erlv8_fun_invocation{}=I, []) ->
	new_AssertionError(I, [""]).


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
				erlv8_vm:equals(VM, Actual, Expected)
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

assert_strictEqual(#erlv8_fun_invocation{ vm = VM }, [Actual, Expected, Message]) ->
	Equals = 
		case Actual of
			#erlv8_object{} ->
				Actual:strict_equals(Expected);
			#erlv8_array{} ->
				Actual:strict_equals(Expected);
			#erlv8_fun{} ->
				Actual:strict_equals(Expected);
			_ ->
				erlv8_vm:strict_equals(VM, Actual, Expected)
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

assert_strictEqual(#erlv8_fun_invocation{}=I, [Actual, Expected]) ->
	assert_strictEqual(I,[Actual, Expected,""]).

assert_notEqual(#erlv8_fun_invocation{ vm = VM }, [Actual, Expected, Message]) ->
	Equals = 
		case Actual of
			#erlv8_object{} ->
				not Actual:equals(Expected);
			#erlv8_array{} ->
				not Actual:equals(Expected);
			#erlv8_fun{} ->
				not Actual:equals(Expected);
			_ ->
				not erlv8_vm:equals(VM, Actual, Expected)
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

assert_notEqual(#erlv8_fun_invocation{}=I, [Actual, Expected]) ->
	assert_notEqual(I,[Actual, Expected,""]).

assert_notStrictEqual(#erlv8_fun_invocation{ vm = VM }, [Actual, Expected, Message]) ->
	Equals = 
		case Actual of
			#erlv8_object{} ->
				not Actual:strict_equals(Expected);
			#erlv8_array{} ->
				not Actual:strict_equals(Expected);
			#erlv8_fun{} ->
				not Actual:strict_equals(Expected);
			_ ->
				not erlv8_vm:strict_equals(VM, Actual, Expected)
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

assert_notStrictEqual(#erlv8_fun_invocation{}=I, [Actual, Expected]) ->
	assert_notStrictEqual(I,[Actual, Expected,""]).


assert_ok(#erlv8_fun_invocation{ vm = VM }, [Guard, Message|_]) ->
	AE = erlv8_vm:retr(VM,{?MODULE,'AssertionError'}),
	Exception = AE:instantiate([?V8Obj([{"actual", Guard}, 
										{"expected", true},
										{"message", Message}])]),
	case Guard of
		false ->
			{throw, Exception};
		null ->
			{throw, Exception};
		undefined ->
			{throw, Exception};
		_ ->
			ok
	end;
		
assert_ok(#erlv8_fun_invocation{}=I, [Guard]) ->
	assert_ok(I, [Guard, ""]).


assert_throws(#erlv8_fun_invocation{ vm = VM }, [Block, Error, Message]) ->
	case Block:call() of
		{throw, _} ->
			ok;
		V ->
			AE = erlv8_vm:retr(VM,{?MODULE,'AssertionError'}),
			Exception = AE:instantiate([?V8Obj([{"actual", V}, 
												{"expected", Error},
												{"message", Message}])]),
			{throw, Exception}
	end;

assert_throws(#erlv8_fun_invocation{}=I, [Block, Error]) ->
	assert_throws(I,[Block, Error, ""]);

assert_throws(#erlv8_fun_invocation{}=I, [Block]) ->
	Global = I:global(),
	Error = Global:get_value("Error"),
	assert_throws(I,[Block, Error, ""]).
	
