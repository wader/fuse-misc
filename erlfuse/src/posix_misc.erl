% Help functions for working with file permissons etc.
% Note that some of the functions make assumptions about the bit format

-module(posix_misc).
-export([is_type/2,
         is_dir/1,
         is_chr/1,
         is_reg/1,
         is_blk/1,
         is_fifo/1,
         is_lnk/1,
         is_sock/1,

         mode/1,
         mode_string/1,
         mode_to_type/1]).

-include("posix_misc.hrl").

% assumes the "other" bit field is the LSBs
-define(MODE_R, ?S_IROTH).
-define(MODE_W, ?S_IWOTH).
-define(MODE_X, ?S_IXOTH).
-define(MODE_MASK, ?S_IRWXO).
-define(MODE_WIDTH, 3).

is_type(Mode, Mask) ->
    (Mode band ?S_IFMT) == Mask.

is_dir(Mode) -> is_type(Mode, ?S_IFDIR).
is_chr(Mode) -> is_type(Mode, ?S_IFCHR). % character device
is_blk(Mode) -> is_type(Mode, ?S_IFBLK). % block device
is_reg(Mode) -> is_type(Mode, ?S_IFREG). % regular file
is_fifo(Mode) -> is_type(Mode, ?S_IFIFO).
is_lnk(Mode) -> is_type(Mode, ?S_IFLNK). % symlink
is_sock(Mode) -> is_type(Mode, ?S_IFSOCK).

perm_to_mode(rwx) -> ?MODE_R bor ?MODE_W bor ?MODE_X;
perm_to_mode(rw) -> ?MODE_R bor ?MODE_W;
perm_to_mode(rx) -> ?MODE_R bor ?MODE_X;
perm_to_mode(r) -> ?MODE_R;
perm_to_mode(wx) -> ?MODE_W bor ?MODE_X;
perm_to_mode(w) -> ?MODE_W;
perm_to_mode(x) -> ?MODE_X.

% mode([reg, {all, r}, {user, w}]) -> 8#100644
mode(L) -> mode_aux(L, 0).
mode_aux([], Acc) -> Acc;
mode_aux([H|T], Acc) when is_integer(H) -> mode_aux(T, Acc bor H);
mode_aux([dir|T], Acc) -> mode_aux(T, Acc bor ?S_IFDIR);
mode_aux([directory|T], Acc) -> mode_aux(T, Acc bor ?S_IFDIR);
mode_aux([chr|T], Acc) -> mode_aux(T, Acc bor ?S_IFCHR);
mode_aux([device|T], Acc) -> mode_aux(T, Acc bor ?S_IFCHR);
mode_aux([blk|T], Acc) -> mode_aux(T, Acc bor ?S_IFBLK);
mode_aux([block|T], Acc) -> mode_aux(T, Acc bor ?S_IFBLK);
mode_aux([reg|T], Acc) -> mode_aux(T, Acc bor ?S_IFREG);
mode_aux([regular|T], Acc) -> mode_aux(T, Acc bor ?S_IFREG);
mode_aux([fifo|T], Acc) -> mode_aux(T, Acc bor ?S_IFIFO);
mode_aux([lnk|T], Acc) -> mode_aux(T, Acc bor ?S_IFLNK);
mode_aux([link|T], Acc) -> mode_aux(T, Acc bor ?S_IFLNK);
mode_aux([sock|T], Acc) -> mode_aux(T, Acc bor ?S_IFSOCK);
mode_aux([socket|T], Acc) -> mode_aux(T, Acc bor ?S_IFSOCK);
mode_aux([{Who, Perm}|T], Acc) ->
    Mode = perm_to_mode(Perm),
    Bits =
        case Who of
        all -> Mode bor (Mode bsl ?MODE_WIDTH) bor (Mode bsl (?MODE_WIDTH * 2));
        user -> Mode bsl (?MODE_WIDTH * 2);
        group -> Mode bsl ?MODE_WIDTH;
        other -> Mode
        end,
    mode_aux(T, Acc bor Bits).

mode_to_perm(Mode) ->
    [if Mode band ?MODE_R == ?MODE_R -> "r"; true -> "-" end,
     if Mode band ?MODE_W == ?MODE_W -> "w"; true -> "-" end,
     if Mode band ?MODE_X == ?MODE_X -> "x"; true -> "-" end].
    
mode_to_type(Mode) ->
    case (Mode band ?S_IFMT) of
    ?S_IFDIR -> directory;
    ?S_IFCHR -> character;
    ?S_IFBLK -> block;
    ?S_IFREG -> regular;
    ?S_IFIFO -> fifo;
    ?S_IFLNK -> link;
    ?S_IFSOCK -> socket;
    _ -> undefined
    end.

% mode to "ls"-mode-string
% mode_string(mode([{all, r}, {user, w}, reg])) ->
% "-rw-r--r--"
mode_string(Mode) ->
    T =
        case mode_to_type(Mode) of
        undefined -> $?;
        % some special cases
        regular -> $-;
        fifo -> $p;
        Type -> hd(atom_to_list(Type))
        end,
    lists:flatten(
        io_lib:format(
            "~c~s~s~s",
            [T,
             mode_to_perm((Mode bsr (?MODE_WIDTH * 2)) band ?MODE_MASK),
             mode_to_perm((Mode bsr ?MODE_WIDTH) band ?MODE_MASK),
             mode_to_perm(Mode band ?MODE_MASK)]
        )
    ).


