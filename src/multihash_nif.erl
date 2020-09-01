-module(multihash_nif).

-export([digest/2, code/1]).

%% Native library support
-export([load/0]).

-on_load(load/0).

-spec digest(binary(), non_neg_integer()) -> {ok, binary()} | {error, term()}.
digest(_Bin, _Hash) ->
    not_loaded(?LINE).

-spec code(binary()) -> {ok, non_neg_integer()} | {error, term()}.
code(_Bin) ->
    not_loaded(?LINE).

%% @private
load() ->
    erlang:load_nif(filename:join(priv(), "libmultihash"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
