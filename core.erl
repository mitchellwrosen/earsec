-module(core).

-export([alt/1,
         alt/2,
         app/1,
         app/2,
         bind/1,
         bind/2,
         empty/0,
         join/1,
         lift/1,
         lift/2,
         lift2/1,
         lift2/3,
         pure/1,
         then/1,
         then/2
        ]).

-include("earsec.hrl").

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
        bind(ParserA, F).
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
        then(ParserA, ParserB).
    end.

-spec then(parser(), parser()) -> parser().
then(ParserA, ParserB) ->
    bind(ParserA, fun(_) -> ParserB end).

% Collapse a parser of parsers into a parser.
-spec join(parser()) -> parser().
join(ParserPA) ->
    bind(ParserPA, fun(ParserA) -> ParserA end).
