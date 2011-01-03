
-record(state, {
        fh_handler,
        ns_handler
        }).

init(Mod) ->
    {ok, Mod}.

open(Opaque, Header, Req) ->
    case Path = fuse_ns_handler:path(Header#fuse_init_in.ino) of
    false -> {error, enoent};
    Path ->
        case Opaque#opaque.mod:open(Path, Req#fuse_open_in.offset
                                       size) of
        {error, Error} = E ->
            E;
        {ok, State} = S ->
            S
        end
    end.

read(Opaque, State, Header, Req) ->
    case Path = fuse_ns_handler:path(Header#fuse_init_in.ino) of
    false -> {error, enoent};
    Path ->
        case Opaque#opaque.mod:read(Path, State, Req#fuse_open_in.offset
                                    size) of
        {error, Error} = E ->
            E;
        {ok, NewState} = S ->
            S
        end
    end.
    

