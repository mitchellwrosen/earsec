-type state()  :: {ok, integer(), binary()} | {error, string(), integer(), binary()}.
-type parser() :: fun((state()) -> {state(), term()}).
-type parse_func() :: fun((integer(), binary()) -> {ok, integer(), binary(), term()}
                                                 | {error, string()}).
