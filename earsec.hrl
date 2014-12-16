-type parser() :: fun((binary()) -> {ok, {term(), integer(), binary()}}
                                  | {error, {term(), integer(), binary()}}).

