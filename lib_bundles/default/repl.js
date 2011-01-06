var erlang = require({module: "beamjs_mod_erlang"});

var that = exports;

exports.start = function () {
	while (that.quitFlag != true) {
		var expr = erlang.apply("io","get_line",["beam.js> "]);
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