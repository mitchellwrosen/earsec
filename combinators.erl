-module(combinators).

-export([between/1,
         choice/1,
         count/1,
         many0/1,
         many1/1,
         option/1,
         optional/1,
         sequence/1
        ]).

-include("earsec.hrl").

% Apply a parser zero or more times.
-spec many0(parser()) -> parser().
many0(Parser) ->
    Lhs = many1(Parser),
    Rhs = core:pure([]),
    (core:alt(Lhs))(Rhs).

% Apply a parser one or more times.
-spec many1(parser()) -> parser().
many1(Parser) ->
    Cons = fun(R) ->
               fun(Rs) ->
                   [R|Rs]
               end
           end,
    ((core:lift2(Cons))(Parser))(many0(Parser)).

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

% Apply parsers in order until one succeeds.
-spec choice([parser()]) -> parser().
choice(Ps) ->
    Step = fun(A, B) -> (core:alt(A))(B) end,
    lists:foldr(Step, core:empty(), Ps).

% Apply a parser N times.
-spec count(integer()) -> fun((parser()) -> parser()).
count(N) ->
    fun(Parser) ->
        sequence(replicate(N, Parser))
    end.

% Run a parser between two others, returning the value parsed by the middle one.
-spec between(parser()) -> fun((parser()) -> fun((parser()) -> parser())).
between(Open) ->
    fun(Between) ->
        fun(Close) ->
            (core:then(Open))(
                (core:bind(Between))(
                    fun(Result) ->
                        (core:then(Close))(
                            core:pure(Result)
                        )
                    end
                )
            )
        end
    end.

% Applies a parser; if it fails, succeed with the given value.
-spec option(term()) -> fun((parser()) -> parser()).
option(Default) ->
    fun(Parser) ->
        (core:alt(Parser))(core:pure(Default))
    end.

% Apply a parser. Whether it succeeds or fails, succeed with 'ok'.
-spec optional(parser()) -> parser().
optional(Parser) ->
    Lhs = (core:then(Parser))(core:pure(ok)),
    Rhs = core:pure(ok),
    (core:alt(Lhs))(Rhs).

%% -----------------------------------------------------------------------------

-spec replicate(integer(), term()) -> [term()].
replicate(N, Term) -> [Term || _ <- lists:seq(1, N)].
