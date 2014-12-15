-module(earsec).

-export([alt/1,
         alt/2,
         app/1,
         app/2,
         between/1,
         between/2,
         between/3,
         bind/1,
         bind/2,
         bytes/1,
         choice/1,
         count/1,
         count/2,
         empty/0,
         end_by0/1,
         end_by0/2,
         end_by1/1,
         end_by1/2,
         join/1,
         lift/1,
         lift/2,
         lift2/1,
         lift2/2,
         lift2/3,
         make_parser/1,
         many0/1,
         many1/1,
         option/1,
         option/2,
         optional/1,
         parse/2,
         pure/1,
         then/1,
         sep_by0/1,
         sep_by1/1,
         sequence/1,
         then/2
        ]).

-include("earsec.hrl").

% Run a parser over input.
%
% Returns:
%   {ok, Result, RemainingInput} on success.
%   {error, Reason, Position, Remainder} on failed parse.
-spec parse(parser(), binary()) -> {ok, term(), binary()} | {error, string(), integer(), binary()}.
parse(Parser, Input) ->
    case Parser({ok, 0, Input}) of
        {{ok, _Position, Remainder}, Result} ->
            {ok, Result, Remainder};
        {Error, _Result} ->
            Error
    end.

% Hide some incidental details of parsers.
-spec make_parser(parse_func()) -> parser().
make_parser(F) ->
    fun
        ({ok, Position1, Remainder1}) ->
            case F(Position1, Remainder1) of
                {ok, Position2, Remainder2, Result} ->
                    {{ok, Position2, Remainder2}, Result};
                Error ->
                    {{Error, Position1, Remainder1}, undefined}
            end;
        (Error) ->
            Error
    end.

% ------------------------------------------------------------------------------
% Core functions
% ------------------------------------------------------------------------------

% Map a function over a parser.
-spec lift(fun((term()) -> term())) -> fun((parser()) -> parser()).
lift(F) ->
    fun(ParserA) ->
        lift(F, ParserA)
    end.

-spec lift(fun((term()) -> term()), parser()) -> parser().
lift(F, ParserA) ->
    fun(State) ->
        case ParserA(State) of
            {{ok, _Position, _Remainder} = State, A} ->
                {State, F(A)};
            Error ->
                Error
        end
    end.


% Map a 2-ary function over a parser.
-spec lift2(fun((term()) -> fun((term()) -> term()))) -> fun((parser()) -> fun((parser()) -> parser())).
lift2(F) ->
    fun(ParserA) ->
        lift2(F, ParserA)
    end.

-spec lift2(fun((term()) -> fun((term()) -> term())), parser()) -> fun((parser()) -> parser()).
lift2(F, ParserA) ->
    fun(ParserB) ->
        lift2(F, ParserA, ParserB)
    end.

-spec lift2(fun((term()) -> fun((term()) -> term())), parser(), parser()) -> parser().
lift2(F, ParserA, ParserB) ->
    app(app(pure(F), ParserA), ParserB).

% Lift a term into an trivial, accepting parser.
-spec pure(term()) -> parser().
pure(Term) ->
    fun(State) ->
        {State, Term}
    end.

% Apply a function parser over a parser.
-spec app(parser()) -> fun((parser()) -> parser()).
app(ParserF) ->
    fun(ParserA) ->
        app(ParserF, ParserA)
    end.

-spec app(parser(), parser()) -> parser().
app(ParserF, ParserA) ->
    bind(ParserF, fun(F) ->
        bind(ParserA, fun(A) ->
            pure(F(A))
        end)
    end).

% Parser that always fails.
-spec empty() -> parser().
empty() ->
    fun({_Success, Position, Remainder}) ->
        {{{error, "empty"}, Position, Remainder}, undefined}
    end.

% Try one parser, and if it fails, try another.
-spec alt(parser()) -> fun((parser()) -> parser()).
alt(ParserA) ->
    fun(ParserB) ->
        alt(ParserA, ParserB)
    end.

-spec alt(parser(), parser()) -> parser().
alt(ParserA, ParserB) ->
    fun(State) ->
        case ParserA(State) of
            {{ok, _Position, _Remainder}, _Result} = Result ->
                Result;
            _ ->
                ParserB(State)
        end
    end.

% Bind the result of one parser to a parser function.
-spec bind(parser()) -> fun((fun((term()) -> parser())) -> parser()).
bind(ParserA) ->
    fun(F) ->
        bind(ParserA, F)
    end.

-spec bind(parser(), fun((term()) -> parser())) -> parser().
bind(ParserA, F) ->
    fun(State1) ->
        case ParserA(State1) of
            {{ok, _Position, _Remainder} = State2, A} ->
                (F(A))(State2);
            Error ->
                Error
        end
    end.

% Run one parser, then toss the result and run a second.
-spec then(parser()) -> fun((parser()) -> parser()).
then(ParserA) ->
    fun(ParserB) ->
        then(ParserA, ParserB)
    end.

-spec then(parser(), parser()) -> parser().
then(ParserA, ParserB) ->
    bind(ParserA, fun(_) -> ParserB end).

% Collapse a parser of parsers into a parser.
-spec join(parser()) -> parser().
join(ParserPA) ->
    bind(ParserPA, fun(ParserA) -> ParserA end).

% ------------------------------------------------------------------------------
% Combinators
% ------------------------------------------------------------------------------

% Apply a parser zero or more times.
-spec many0(parser()) -> parser().
many0(Parser) ->
    earsec_core:alt(many1(Parser), earsec_core:pure([])).

% Apply a parser one or more times.
-spec many1(parser()) -> parser().
many1(Parser) ->
    core:lift2(cons(), Parser, many0(Parser)).

% Sequence a (non-empty) list of parsers.
-spec sequence([parser()]) -> parser().
sequence([P]) ->
    core:lift(fun(R) -> [R] end, P);
sequence([P|Ps]) ->
    core:lift2(cons(), P, sequence(Ps)).

% Apply parsers in order until one succeeds.
-spec choice([parser()]) -> parser().
choice(Ps) ->
    lists:foldr(fun core:alt/2, core:empty(), Ps).

% Apply a parser N times.
-spec count(integer()) -> fun((parser()) -> parser()).
count(N) ->
    fun(Parser) ->
        count(N, Parser)
    end.

-spec count(integer(), parser()) -> parser().
count(N, Parser) ->
    sequence(replicate(N, Parser)).

% Run a parser between two others, returning the value parsed by the middle one.
-spec between(parser()) -> fun((parser()) -> fun((parser()) -> parser())).
between(Open) ->
    fun(Between) ->
        between(Open, Between)
    end.

-spec between(parser(), parser()) -> fun((parser()) -> parser()).
between(Open, Between) ->
    fun(Close) ->
        between(Open, Between, Close)
    end.

-spec between(parser(), parser(), parser()) -> parser().
between(Open, Between, Close) ->
    core:then(Open,
        core:bind(Between,
            fun(Result) ->
                core:then(Close, core:pure(Result))
            end
        )
    ).

% Applies a parser; if it fails, succeed with the given value.
-spec option(term()) -> fun((parser()) -> parser()).
option(Default) ->
    fun(Parser) ->
        option(Default, Parser)
    end.

-spec option(term(), parser()) -> parser().
option(Default, Parser) ->
    core:alt(Parser, core:pure(Default)).

% Apply a parser. Whether it succeeds or fails, succeed with 'ok'.
-spec optional(parser()) -> parser().
optional(Parser) ->
    PureOk = core:pure(ok),
    core:alt(core:then(Parser, PureOk), PureOk).

% Parse zero or more times, each separated by another parser.
-spec sep_by0(parser()) -> fun((parser()) -> parser()).
sep_by0(Parser) ->
    fun(Sep) ->
        sep_by0(Parser, Sep)
    end.

-spec sep_by0(parser(), parser()) -> parser().
sep_by0(Parser, Sep) ->
    core:alt(sep_by1(Parser, Sep), core:pure([])).

% Parse one or more times, each separated by another parser.
-spec sep_by1(parser()) -> fun((parser()) -> parser()).
sep_by1(Parser) ->
    fun(Sep) ->
        sep_by1(Parser, Sep)
    end.

-spec sep_by1(parser(), parser()) -> parser().
sep_by1(Parser, Sep) ->
    core:bind(Parser,
        fun(X) ->
            XsParser = many0(core:then(Sep, Parser)),
            core:bind(XsParser,
                fun(Xs) ->
                    core:pure([X|Xs])
                end
            )
        end
    ).

-spec end_by0(parser()) -> fun((parser()) -> parser()).
end_by0(Parser) ->
    fun(Sep) ->
        end_by0(Parser, Sep)
    end.

-spec end_by0(parser(), parser()) -> parser().
end_by0(Parser, Sep) ->
    many0(endby_body(Parser, Sep)).

-spec end_by1(parser()) -> fun((parser()) -> parser()).
end_by1(Parser) ->
    fun(Sep) ->
        end_by1(Parser, Sep)
    end.

-spec end_by1(parser(), parser()) -> parser().
end_by1(Parser, Sep) ->
    many1(endby_body(Parser, Sep)).

endby_body(Parser, Sep) ->
    core:bind(Parser,
        fun(Ret) ->
            core:then(Sep, core:pure(Ret))
        end).

% -----------------------------------------------------------------------------
% Char functions
% -----------------------------------------------------------------------------

% Parse the specified number of bytes.
-spec bytes(integer()) -> parser().
bytes(N) -> make_parser:bytes_(N).

-spec bytes_(integer()) -> parse_func().
bytes_(Bytes) ->
    Bits = Bytes*8,
    fun
        (Position, <<N:Bits, Remainder/binary>>) ->
            {ok, Position + Bytes, Remainder, N};
        (_, _) ->
            {error, "bytes"}
    end.

% -----------------------------------------------------------------------------
% Misc
% -----------------------------------------------------------------------------

cons() ->
    fun(X) ->
        fun(Xs) ->
            [X|Xs]
        end
    end.

-spec replicate(integer(), term()) -> [term()].
replicate(N, Term) -> [Term || _ <- lists:seq(1, N)].
