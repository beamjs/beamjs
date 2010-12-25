exports = new (require('events').EventEmitter)();
exports.exit = function() {
        require('repl').quit();
}
