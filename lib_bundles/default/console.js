var erlang = require({module: "beamjs_mod_erlang"});

exports.error = exports.warn = exports.info = exports.log = function(d) {
	if (d == null) {
		erlang.apply("io","format",["null~n"]);
	} else if (typeof d == 'function') {
		erlang.apply("io","format",["~s~n",[d.toString()]]);
	} else if (typeof d == 'object') {
		erlang.apply("io","format",["~s~n",[d.toString()]]);
	} else if (typeof d == 'string') {
		erlang.apply("io","format",["~s~n",[d]]);
	} else if (typeof d == 'number') {
		erlang.apply("io","format",["~w~n",[d]]);
	}
}