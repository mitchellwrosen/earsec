-module(combinators).

-export([between/1,
         between/2,
         between/3,
         choice/1,
         count/1,
         count/2,
         end_by0/1,
         end_by0/2,
         end_by1/1,
         end_by1/2,
         many0/1,
         many1/1,
         option/1,
         option/2,
         optional/1,
         sep_by0/1,
         sep_by1/1,
         sequence/1
        ]).

-include("earsec.hrl").

% Apply a parser zero or more times.
-spec many0(parser()) -> parser().
many0(Parser) ->
    core:alt(many1(Parser), core:pure([])).

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

%% -----------------------------------------------------------------------------

cons() ->
    fun(X) ->
        fun(Xs) ->
            [X|Xs]
        end
    end.

-spec replicate(integer(), term()) -> [term()].
replicate(N, Term) -> [Term || _ <- lists:seq(1, N)].
