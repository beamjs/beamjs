var gen_event = require({module: "beamjs_mod_gen_event"});

exports.EventEmitter = function() {
	this._eventManager = new gen_event.Manager();
	this._eventListeners = {};
};

addListener = function(event, listener) {
 	var that = this;
	var f = function (arguments) {
		args = Array.apply(this, arguments);
		if (that._eventListeners[event].indexOf(listener) == -1)  {
			return this.RemoveHandler;
		} else if (args.shift() == event) {
			listener.apply(this,args);
		}
	};
	f._original = listener;
	this._eventManager.addHandler(new gen_event.Handler(f));
	this.emit("newListener", event, listener);
}


exports.EventEmitter.prototype = {
	emit: function () {
		var _event = arguments[0];
		// check _eventListeners consistency
		var _handlers = this._eventManager.whichHandlers();
		for (i in this._eventListeners[_event]) {
			var _listener = this._eventListeners[_event][i];
			var _found = false;
			for (j in _handlers) {
				var _handler = _handlers[j];
				if ((_found = (_handler._onEvent._original == _listener)))
					break;
			}
			if (!_found) {
				addListener.call(this,_event,_listener);
			}
		}
		// prepare event
		args = [];
		for (k in arguments) {
			args[k] = arguments[k];
		}
		// send it off
		this._eventManager.notify(args);
	},
	addListener: function(event, listener) {
		if (this._eventListeners[event] == undefined)
			this._eventListeners[event] = [];
		this._eventListeners[event].push(listener);
		addListener.call(this,event,listener);
	},
	once: function(event, listener) {
		var that = this;
		this.addListener(event, function () {
							 listener.apply(this,args);
							 that._eventListeners[event].splice(that._eventListeners[event].indexOf(listener),1);
							 return this.RemoveHandler;
						 });
	},
	listeners: function(event) {
		if (this._eventListeners[event] == undefined)
			this._eventListeners[event] = [];
		return this._eventListeners[event];
	},
	removeListener: function(listener) {
		for (event in this._eventListeners) {
			if ((i=this._eventListeners[event].indexOf(listener)) > -1) {
				this._eventListeners[event].splice(i,1);
			}
		}
	},
	removeAllListeners: function(event) {
		this._eventListeners[event] = [];
	}
};

exports.EventEmitter.prototype.on = exports.EventEmitter.prototype.addListener;