-module(combinators).

-export([choice/1]).

% Try one parser, and if it fails, try another.
-spec choice(parser()) -> fun((parser()) -> parser()).
choice(ParserA) ->
    fun(ParserB) ->
        fun(Input) ->
            case ParserA(Input) of
                #state{success = ok} = State ->
                    State;
                _ ->
                    ParserB(Input)
            end
        end
    end.
