-module(beamjs_mod_fs).
-export([exports/1,init/1, errno_desc/1]).
-behaviour(erlv8_module).
-include_lib("erlv8/include/erlv8.hrl").

init(_VM) ->
	ok.

exports(_VM) ->
    ?V8Obj([
            {"readFile", fun read_file/2},
            {"readdir", erlv8_fun:new(fun list_dir/2,
                    ?V8Obj([{ "__doc__",
                                "`readdir(path, callback)`\n\n"
                                "Asynchronous readdir(3). Reads the contents of a directory.\n\n"
                                "The callback gets two arguments (err, files) where files is "
                                "an array of the names of the files in the directory excluding '.' and '..'."
                            }]))},
            {"rmdir", erlv8_fun:new(fun del_dir/2,
                    ?V8Obj([{ "__doc__",
                                "`rmdir(path, callback)`\n\n"
                                "Asynchronous [rmdir(3)](http://manpages.ubuntu.com/manpages/maverick/en/man3/rmdir.3posix.html)."
                                "No arguments other than a possible exception are given to the completion callback."
                            }]))}
        ]).

read_file(#erlv8_fun_invocation{},[Filename, Encoding, #erlv8_fun{} = Callback]) when is_list(Filename) andalso is_list(Encoding)  ->
	case file:read_file(Filename) of
		{ok, B} when is_binary(B) ->
			Callback:call([null, binary_to_list(B)]);
		{error, Errno} ->
                        Callback:call([{error, Filename ++ ": " ++ errno_desc(Errno)},""])
	end;

read_file(#erlv8_fun_invocation{}=I,[Filename, #erlv8_fun{} = Callback]) when is_list(Filename)  ->
	read_file(I,[Filename, "utf-8", Callback]);
read_file(#erlv8_fun_invocation{},[Filename, Encoding]) when is_list(Filename) andalso is_list(Encoding)  ->
	{throw, {error, "No callback specified"}}.

list_dir(#erlv8_fun_invocation{}, [Path, #erlv8_fun{} = Callback]) when is_list(Path) ->
    case file:list_dir(Path) of
        { ok, List } ->
            Callback:call([null, ?V8Arr(List)]);
        { error, Errno } ->
            Callback:call([{ error, Path ++ ": " ++ errno_desc(Errno)},""])
    end.

del_dir(#erlv8_fun_invocation{}, [Path, #erlv8_fun{} = Callback]) when is_list(Path) ->
    case file:del_dir(Path) of
        ok ->
            Callback:call([null]);
        { error, Errno } ->
            Callback:call([{ error, Path ++ ": " ++ errno_desc(Errno)},""])
    end.


% POSIX Error descriptions
errno_desc(eacces)  -> "permission denied";
errno_desc(eagain)  -> "resource temporarily unavailable";
errno_desc(ebadf)   -> "bad file number";
errno_desc(ebusy)   -> "file busy";
errno_desc(edquot)  -> "disk quota exceeded";
errno_desc(eexist)  -> "file already exists";
errno_desc(efault)  -> "bad address in system call argument";
errno_desc(efbig)   -> "file too large";
errno_desc(eintr)   -> "interrupted system call";
errno_desc(einval)  -> "invalid argument";
errno_desc(eio)     -> "IO error";
errno_desc(eisdir)  -> "illegal operation on a directory";
errno_desc(eloop)   -> "too many levels of symbolic links";
errno_desc(emfile)  -> "too many open files";
errno_desc(emlink)  -> "too many links";
errno_desc(enfile)  -> "file table overflow";
errno_desc(enodev)  -> "no such device";
errno_desc(enoent)  -> "no such file or directory";
errno_desc(enomem)  -> "not enough memory";
errno_desc(enospc)  -> "no space left on device";
errno_desc(enotblk) -> "block device required";
errno_desc(enotdir) -> "not a directory";
errno_desc(enotsup) -> "operation not supported";
errno_desc(enxio)   -> "no such device or address";
errno_desc(eperm)   -> "not owner";
errno_desc(epipe)   -> "broken pipe";
errno_desc(erofs)   -> "read-only file system";
errno_desc(espipe)  -> "invalid seek";
errno_desc(esrch)   -> "no such process";
errno_desc(estale)  -> "stale remote file handle";
errno_desc(exdev)   -> "cross-domain link";
errno_desc(enametoolong) -> "file name too long".
