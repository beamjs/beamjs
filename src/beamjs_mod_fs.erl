-module(beamjs_mod_fs).
-export([exports/1,init/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
    ?V8Obj([
            {"readFile", fun read_file/2},
            {"readdir", erlv8_fun:new(fun read_dir/2,
                    ?V8Obj([{ "__doc__",
                                "`readdir(path, callback)`\n\n"
                                "Asynchronous readdir(3). Reads the contents of a directory.\n\n"
                                "The callback gets two arguments (err, files) where files is "
                                "an array of the names of the files in the directory excluding '.' and '..'."
                            }]))}
        ]).

read_file(#erlv8_fun_invocation{},[Filename, Encoding, #erlv8_fun{} = Callback]) when is_list(Filename) andalso is_list(Encoding)  ->
	case file:read_file(Filename) of
		{ok, B} when is_binary(B) ->
			Callback:call([null, binary_to_list(B)]);
		{error, noent} ->
			Callback:call([{error, "File not found"},""])
	end;

read_file(#erlv8_fun_invocation{}=I,[Filename, #erlv8_fun{} = Callback]) when is_list(Filename)  ->
	read_file(I,[Filename, "utf-8", Callback]);
read_file(#erlv8_fun_invocation{},[Filename, Encoding]) when is_list(Filename) andalso is_list(Encoding)  ->
	{throw, {error, "No callback specified"}}.

read_dir(#erlv8_fun_invocation{}, [Path, #erlv8_fun{} = Callback]) when is_list(Path) ->
    case file:list_dir(Path) of
        { ok, List } ->
            Callback:call([null, ?V8Arr(List)]);
        { error, enoent } ->
            Callback:call([{error, "No such directory"},""]);
        { error, _ } ->
            Callback:call([{error, "Cannot read directory"},""])
    end.
