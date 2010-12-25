
/*!
 * JUnify - Unification JavaScript Library v0.22
 *
 * Licensed under the revised BSD License.
 * Copyright 2008, Bram Stein
 * All rights reserved.
 */

exports =  function () {
	var _ = 0xBAADF00D;

	function is_array(value) {
		return value && 
			typeof value === 'object' &&
			typeof value.length === 'number' &&
			typeof value.splice === 'function' &&
			!value.propertyIsEnumerable('length');
	}

	function is_object(value) {
		return value && 
			typeof value === 'object' && 
			!is_array(value);
	}

	function is_function(value) {
		return value && 
			typeof value === 'function';
	}

	function is_boolean(value) {
		return value !== null && 
			typeof value === 'boolean';
	}

	function is_atom(value) {
		return ((typeof value !== 'object' || value === null) && 
			typeof value !== 'function') || 
			is_boolean(value);
	}

	function is_variable(value) {
		return value &&
			typeof value === 'object' &&
			typeof value.is_variable === 'function' &&
			typeof value.get_name === 'function' &&
			value.is_variable();
	}

	function is_wildcard(value) {
		return value && value === _;
	}

	function variable(value, type) {
		return {
			is_variable : function () {
				return true;
			},
			get_name : function () {
				return value;
			},
			get_type : function () {
				return type || false;
			}
		};
	}

	function occurs(variable, pattern) {
		var i;
		var key;

		if (is_variable(pattern) && variable.get_name() === pattern.get_name()) {
			return true;
		}
		else if (is_variable(pattern) || is_atom(pattern)) {
			return false;
		}
		else if (is_array(pattern)) {
			for (i = 0; i < pattern.length; i += 1) {
				if (occurs(variable, pattern[i])) {
					return true;
				}
			}
			return false;
		}
		else if (is_object(pattern)) {
			for (key in pattern) {
				if (pattern.hasOwnProperty(key) && !is_function(pattern[key])) {
					if (occurs(variable, pattern[key])) {
						return true;
					}
				}
			}
			return false;
		}
	}

	function get_binding(variable, substitution) {
		var binding;

		if (substitution.hasOwnProperty(variable.get_name())) {			
			binding = {};
			binding[variable.get_name()] = substitution[variable.get_name()];
		}
		return binding;
	}

	function add_substitution(variable, pattern, substitution) {
		substitution[variable.get_name()] = pattern;
		return substitution;
	}

	function match_var(variable, pattern, substitution) {
		var binding;

		// don't match a variable with another (or itself)
		if (is_variable(pattern) && is_variable(variable)) {
			return false;
		}
		// if the variable or pattern is a wildcard we return without binding
		else if (is_wildcard(variable) || is_wildcard(pattern)) {
			return substitution;
		}
		// if the variable has a type which doesn't match the type of the pattern,
		// we return false (no match)
		else if (variable.get_type() !== false && variable.get_type() !== pattern.constructor) {
			return false;
		}
		// otherwise we try to bind the pattern to the variable
		else {
			binding = get_binding(variable, substitution);
			// if it's already bound we call unify again to resolve any variables inside
			// the binding.
			if (binding) {
				return unify_aux(binding[variable.get_name()], pattern, substitution);
			}
			if (occurs(variable, pattern)) {
				return false;
			}
			else {
				return add_substitution(variable, pattern, substitution);
			}
		}
	}

	function unify_object(pattern1, pattern2, substitution) {
		var has_var, key, c1, c2;

		has_var = pattern2.hasOwnProperty("_");
		c1 = c2 = 0;

		for (key in pattern1) {
			if (pattern1.hasOwnProperty(key) && !is_function(pattern1[key])) {
				if (key !== "_") {
					if (pattern2.hasOwnProperty(key)) {
						if (unify_aux(pattern1[key], pattern2[key], substitution) === false) {
							return false;
						}
					}
					else if (!pattern2.hasOwnProperty(key) && !has_var) {
						return false;
					}
				}
				c1 += 1;
			}
		}

		has_var = pattern1.hasOwnProperty("_");

		for (key in pattern2) {
			if (pattern2.hasOwnProperty(key) && !is_function(pattern2[key])) {
				if (key !== "_") {
					if (pattern1.hasOwnProperty(key)) {
						if (unify_aux(pattern1[key], pattern2[key], substitution) === false) {
							return false;
						}
					}
					else if (!pattern1.hasOwnProperty(key) && !has_var) {
						return false;
					}
				}
				c2 += 1;
			}
		}

		if (c1 === 0 && c2 === 0) {
			return substitution;
		}
		if (c1 === 0 || c2 === 0) {
			return false;
		}
		return substitution;
	}

	function unify_array(pattern1, pattern2, substitution) {
		var i;

		if (pattern1.length === pattern2.length) {
			for (i = 0; i < pattern1.length; i += 1) {
				if (unify_aux(pattern1[i], pattern2[i], substitution) === false) {
					return false;
				}
			}
		}
		return substitution;
	}

	function unify_aux(pattern1, pattern2, substitution) {	
		if (substitution === false) {
			return false;
		}
		else if (is_variable(pattern1) || is_wildcard(pattern1)) {
			return match_var(pattern1, pattern2, substitution);
		}
		else if (is_variable(pattern2) || is_wildcard(pattern2)) {
			return match_var(pattern2, pattern1, substitution);
		}
		else if (is_atom(pattern1)) {
			if (pattern1 === pattern2) {
				return substitution;
			}
			else {
				return false;
			}
		}
		else if (is_atom(pattern2)) {
			return false;
		}
		else if (is_array(pattern1) && is_array(pattern2)) {
			return unify_array(pattern1, pattern2, substitution);
		}
		else if (is_object(pattern1) && is_object(pattern2)) {
			return unify_object(pattern1, pattern2, substitution);
		}
	}

	function visit_pattern(pattern, visitor) {
		var key, i, value;

		if (is_variable(pattern)) {
			return (visitor.hasOwnProperty('variable') && visitor.variable(pattern)) || pattern;
		}
		else if (is_wildcard(pattern)) {
			return (visitor.hasOwnProperty('wildcard') && visitor.wildcard()) || pattern;
		}
		else if (is_function(pattern)) {
			return (visitor.hasOwnProperty('func') && visitor.func(pattern)) || pattern;
		}
		else if (is_object(pattern)) {
			for (key in pattern) {
				if (pattern.hasOwnProperty(key) && visitor.hasOwnProperty('object') && key !== _) {
					value = visitor.object(key, pattern[key]);
					pattern[key] = visit_pattern(value, visitor);
				}
				else if (pattern.hasOwnProperty(key) && key !== _) {
					pattern[key] = visit_pattern(pattern[key], visitor);
				}
			}
			return pattern;
		}
		else if (is_array(pattern)) {
			for (i = 0; i < pattern.length; i += 1) {
				pattern[i] = visit_pattern(pattern[i], visitor);
			}
			return pattern;
		}
		else {
			return (visitor.hasOwnProperty('atom') && visitor.atom(pattern)) || pattern;
		}
	}

	return {
		unify : function (pattern1, pattern2) {
			return unify_aux(pattern1, pattern2, {});
		},
		visit_pattern : function (pattern, visitor) {
			return visit_pattern(pattern, visitor);
		},
		variable : variable,
		_ : _
	};
}();
