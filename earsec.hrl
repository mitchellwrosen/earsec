-record(state,
        { success   :: ok | {error, atom()}, % Whether or not the parser has failed.
          result    :: term(),
          position  :: integer(),
          remainder :: binary()
        }).

-type parser() :: fun((binary()) -> #state{} | {error, atom()}).
