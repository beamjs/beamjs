exports = new (require('events').EventEmitter)();
exports.exit = function() {
        require('repl').quit();
}
exports.ENV = require({ module: 'beamjs_mod_os' }).getFullEnv();
exports.env = exports.ENV;
