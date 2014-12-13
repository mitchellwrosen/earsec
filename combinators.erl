-module(combinators).

-export([choice/1,
         sequence/1
        ]).

-include("earsec.hrl").

% Try one parser, and if it fails, try another.
-spec choice(parser()) -> fun((parser()) -> parser()).
choice(ParserA) ->
    fun(ParserB) ->
        fun(State) ->
            case ParserA(State) of
                {{ok, _, _}, _} = Result ->
                    Result;
                _ ->
                    ParserB(State)
            end
        end
    end.

% Sequence a (non-empty) list of parsers.
-spec sequence([parser()]) -> parser().
sequence([P]) ->
    F = fun(R) -> [R] end,
    (core:lift(F))(P);
sequence([P|Ps]) ->
    F = fun(R) ->
            fun(Rs) ->
                [R|Rs]
            end
        end,
    ((core:lift2(F))(P))(sequence(Ps)).
