-module(earsec).

-export([alt/2,
         app/2,
         between/3,
         binary/1,
         bind/2,
         choice/1,
         count/2,
         empty/0,
         end_by0/2,
         end_by1/2,
         join/1,
         lift/2,
         lift2/3,
         lift3/4,
         many0/1,
         many1/1,
         option/2,
         optional/1,
         parse/2,
         pure/1,
         sep_by0/2,
         sep_by1/2,
         sequence/1,
         then/2,
         uint8/0,
         uint16/0,
         uint32/0,
         uint64/0
        ]).

-export_type([parser/1]).

-type parser(Term) :: fun((binary()) -> {ok, {Term, integer(), binary()}}
                                      | {error, {Term, integer(), binary()}}).

% Run a parser over input.
-spec parse(parser(A :: term()), binary()) -> {ok, A} | {error, {A, integer(), binary()}}.
parse(Parser, Input) ->
    case Parser(Input) of
        {ok, {Result, _, <<>>}} ->
            {ok, Result};
        {ok, {_, Position, Remainder}} ->
            {error, {trailing_input, Position, Remainder}};
        Error ->
            Error
    end.

% ------------------------------------------------------------------------------
% Core functions
% ------------------------------------------------------------------------------

% Map a function over a parser.
-spec lift(fun((A) -> B), parser(A)) -> parser(B).
lift(F, ParserA) ->
    app(pure(F), ParserA).

% Map a 2-ary function over a parser.
-spec lift2(fun((A, B) -> C), parser(A), parser(B)) -> parser(C).
lift2(F, ParserA, ParserB) ->
    app(app(pure(uncurry(F)), ParserA), ParserB).

-spec lift3(fun((A, B, C) -> D), parser(A), parser(B), parser(C)) -> parser(D).
lift3(F, ParserA, ParserB, ParserC) ->
    app(app(app(pure(uncurry2(F)), ParserA), ParserB), ParserC).

% Lift a term into an trivial, accepting parser.
-spec pure(A) -> parser(A).
pure(Term) ->
    fun(Input) ->
        {ok, {Term, 0, Input}}
    end.

% Apply a function parser over a parser.
-spec app(parser(fun((A) -> B)), parser(A)) -> parser(B).
app(ParserF, ParserA) ->
    bind(ParserF, fun(F) ->
        bind(ParserA, fun(A) ->
            pure(F(A))
        end)
    end).

% Parser that always fails.
-spec empty() -> parser(A) when A :: term().
empty() ->
    fun(Input) ->
        {error, {"empty", 0, Input}}
    end.

% Try one parser, and if it fails, try another.
-spec alt(parser(A :: term()), parser(A)) -> parser(A).
alt(ParserA, ParserB) ->
    fun(Input) ->
        case ParserA(Input) of
            {ok, _} = Result ->
                Result;
            _ ->
                ParserB(Input)
        end
    end.

% Bind the result of one parser to a parser function.
-spec bind(parser(A), fun((A) -> parser(B))) -> parser(B).
bind(ParserA, F) ->
    fun(Input1) ->
        case ParserA(Input1) of
            {ok, {A, Position1, Input2}} ->
                {Success, {Result, Position2, Input3}} = (F(A))(Input2),
                % No matter if success or failure; offset the position by
                % the result of the first parser
                {Success, {Result, Position1 + Position2, Input3}};
            Error ->
                Error
        end
    end.

% Run one parser, then toss the result and run a second.
-spec then(parser(A), parser(B)) -> parser(B) when A :: term().
then(ParserA, ParserB) ->
    bind(ParserA, fun(_) -> ParserB end).

% Collapse a parser of parsers into a parser.
-spec join(parser(Parser)) -> Parser when Parser :: parser(A), A :: term().
join(ParserPA) ->
    bind(ParserPA, fun(ParserA) -> ParserA end).

% ------------------------------------------------------------------------------
% Combinators
% ------------------------------------------------------------------------------

% Apply a parser zero or more times.
-spec many0(parser(A)) -> parser([A]).
many0(Parser) ->
    alt(many1(Parser), pure([])).

% Apply a parser one or more times.
-spec many1(parser(A)) -> parser([A]).
many1(Parser) ->
    lift2(fun cons/2, Parser, many0(Parser)).

% Sequence a (non-empty) list of parsers.
-spec sequence([parser(A)]) -> parser([A]).
sequence([P]) ->
    lift(fun(R) -> [R] end, P);
sequence([P|Ps]) ->
    lift2(fun cons/2, P, sequence(Ps)).

% Apply parsers in order until one succeeds.
-spec choice([parser(A)]) -> parser(A).
choice(Ps) ->
    lists:foldr(fun alt/2, empty(), Ps).

% Apply a parser N times.
-spec count(integer(), parser(A)) -> parser([A]).
count(N, Parser) ->
    sequence(replicate(N, Parser)).

% Run a parser between two others, returning the value parsed by the middle one.
-spec between(parser(A), parser(B), parser(C)) -> parser(B) when A :: term(), C :: term().
between(Open, Between, Close) ->
    then(Open,
        bind(Between,
            fun(Result) ->
                then(Close, pure(Result))
            end
        )
    ).

% Applies a parser; if it fails, succeed with the given value.
-spec option(A, parser(A)) -> parser(A).
option(Default, Parser) ->
    alt(Parser, pure(Default)).

% Apply a parser. Whether it succeeds or fails, succeed with 'ok'.
-spec optional(parser(A)) -> parser(ok) when A :: term().
optional(Parser) ->
    PureOk = pure(ok),
    alt(then(Parser, PureOk), PureOk).

% Parse zero or more times, each separated by another parser.
-spec sep_by0(parser(A), parser(B)) -> parser([A]) when B :: term().
sep_by0(Parser, Sep) ->
    alt(sep_by1(Parser, Sep), pure([])).

% Parse one or more times, each separated by another parser.
-spec sep_by1(parser(A), parser(B)) -> parser([A]) when B :: term().
sep_by1(Parser, Sep) ->
    bind(Parser,
        fun(X) ->
            XsParser = many0(then(Sep, Parser)),
            bind(XsParser,
                fun(Xs) ->
                    pure([X|Xs])
                end
            )
        end
    ).

-spec end_by0(parser(A), parser(B)) -> parser([A]) when B :: term().
end_by0(Parser, Sep) ->
    many0(endby_body(Parser, Sep)).

-spec end_by1(parser(A), parser(B)) -> parser([A]) when B :: term().
end_by1(Parser, Sep) ->
    many1(endby_body(Parser, Sep)).

endby_body(Parser, Sep) ->
    bind(Parser,
        fun(Ret) ->
            then(Sep, pure(Ret))
        end).

% -----------------------------------------------------------------------------
% Char functions
% -----------------------------------------------------------------------------

% Parse an unsigned, big-endian integer of the specified byte size.
-spec uint(integer(), string()) -> parser(non_neg_integer()).
uint(Bytes, ErrReason) ->
    Bits = Bytes*8,
    fun
        (<<N:Bits, Rest/binary>>) ->
            {ok, {N, Bytes, Rest}};
        (Rest) ->
            {error, {ErrReason, 0, Rest}}
    end.

-spec uint8() -> parser(non_neg_integer()).
uint8() -> uint(1, "uint8").

-spec uint16() -> parser(non_neg_integer()).
uint16() -> uint(2, "uint16").

-spec uint32() -> parser(non_neg_integer()).
uint32() -> uint(4, "uint32").

-spec uint64() -> parser(non_neg_integer()).
uint64() -> uint(8, "uint64").

% Parse a binary of the specified length.
-spec binary(non_neg_integer()) -> parser(binary()).
binary(Bytes) ->
    fun
        (<<Binary:Bytes/binary, Rest/binary>>) ->
            {ok, {Binary, Bytes, Rest}};
        (Rest) ->
            {error, {"binary", 0, Rest}}
    end.

% -----------------------------------------------------------------------------
% Misc
% -----------------------------------------------------------------------------

cons(X, Xs) -> [X|Xs].

-spec replicate(integer(), term()) -> [term()].
replicate(N, Term) -> [Term || _ <- lists:seq(1, N)].

-spec uncurry(fun((term(), term()) -> term())) -> fun((term()) -> fun((term()) -> term())).
uncurry(F) ->
    fun(A) ->
        fun(B) ->
            F(A, B)
        end
    end.

-spec uncurry2(fun((term(), term(), term()) -> term())) -> fun((term()) -> fun((term()) -> fun((term()) -> term()))).
uncurry2(F) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                F(A, B, C)
            end
        end
    end.
