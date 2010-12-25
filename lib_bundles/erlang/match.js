var unification = require('unification');


/*!
 * 
 * Based on work by Bram Stein:
 * 
 * JavaScript Pattern Matching
 *
 * Licensed under the revised BSD License.
 * Copyright 2008, Bram Stein
 * All rights reserved.
 */

exports = function () {
    var unify = unification.unify,
        slice = Array.prototype.slice;

    function match_aux(patterns, value) {
        var i, result, length;

        for (i = 0; i < patterns.length; i += 1) {
            length = patterns[i].length;
   
            // we only try to match if the match array contains at
			// least two items and the last item is a function (closure)
            if (length >= 2 && typeof patterns[i][length - 1] === 'function') {
                result = unify(patterns[i].slice(0, length - 1), value);
                if (result) {
                    return patterns[i][length - 1](result);
                }
            }
        }
        return undefined;
    }

	function match() {
		var args = slice.apply(arguments);
		if (typeof this.args == 'object')
			args = this.args.concat(args);
        f = function() {
            return match_aux(args, slice.apply(arguments));
        };
		f.match = match;
		f.args = args;
		return f;
    };
	return match;
}();

exports.var = unification.variable
exports._ = unification._