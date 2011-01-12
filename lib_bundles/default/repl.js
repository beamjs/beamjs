var erlang = require({module: "beamjs_mod_erlang"});

var that = exports;

exports.start = function () {
	var node = require({module: "beamjs_mod_dist"}).node();
	var prompt = (node == "nonode@nohost" ? "" : "(" + node + ")") + "beam.js> ";
	erlang.apply("io","get_line",[prompt], function(expr) {
					 try {
						 var result = beamjs.VM.current.run(expr);
						 if (result != null) {
							 console.log(result);
						 }
					 } catch (x) {
						 console.log(x.stack);
					 }
					 that.start();
				 });
};
			 