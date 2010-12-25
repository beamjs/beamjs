// http://nodejs.org/docs/v0.3.2/api/os.html
exports.getHostname = function() {
    return require({module: 'beamjs_mod_dist' }).localhost();
}
exports.getHostname.__doc__ = 'Returns the hostname of the operating system.'
exports.__doc__ = "[Node.js](http://nodejs.org/docs/v0.3.2/api/os.html) compatible `os` module"
