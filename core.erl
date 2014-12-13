-module(core).

-export([app/1,
         bind/1,
         join/1,
         lift/1,
         lift2/1,
         pure/1,
         then/1
        ]).

-include("earsec.hrl").

% Map a function over a parser.
-spec lift(fun((term()) -> term())) -> fun((parser()) -> parser()).
lift(F) ->
    fun(ParserA) ->
        fun(State) ->
            case ParserA(State) of
                {{ok, _Position, _Remainder} = State, A} ->
                    {State, F(A)};
                Error ->
                    Error
            end
        end
    end.

% Map a 2-ary function over a parser.
-spec lift2(fun((term()) -> fun((term()) -> term()))) -> fun((parser()) -> fun((parser()) -> parser())).
lift2(F) ->
    fun(ParserA) ->
        fun(ParserB) ->
            ParserF  = pure(F),
            ParserBC = (app(ParserF))(ParserA),
            (app(ParserBC))(ParserB)
        end
    end.

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
        (bind(ParserF))(fun(F) ->
            (bind(ParserA))(fun(A) ->
                pure(F(A))
            end)
        end)
    end.

% Bind the result of one parser to a parser function.
-spec bind(parser()) -> fun((fun((term()) -> parser())) -> parser()).
bind(ParserA) ->
    fun(F) ->
        fun(State1) ->
            case ParserA(State1) of
                {{ok, _Position, _Remainder} = State2, A} ->
                    (F(A))(State2);
                Error ->
                    Error
            end
        end
    end.

% Run one parser, then toss the result and run a second.
-spec then(parser()) -> fun((parser()) -> parser()).
then(ParserA) ->
    fun(ParserB) ->
        (bind(ParserA))(fun(_) -> ParserB end)
    end.

% Collapse a parser of parsers into a parser.
-spec join(parser()) -> parser().
join(ParserPA) ->
    (bind(ParserPA))(fun(ParserA) -> ParserA end).
