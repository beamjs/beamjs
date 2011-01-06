var erlang = require({module: "beamjs_mod_erlang"});

var that = exports;

exports.start = function () {
	var node = require({module: "beamjs_mod_dist"}).node();
	var prompt = (node == "nonode@nohost" ? "" : "(" + node + ")") + "beam.js> ";
	while (that.quitFlag != true) {
		var expr = erlang.apply("io","get_line",[prompt]);
		try {
			var result = beamjs.VM.current.run(expr);
			if (result != null) {
				console.log(result);
			}
		} catch (x) {
			console.log(x.stack);
		}
	}
}
			  
exports.quit = function() {
	that.quitFlag = true;
}