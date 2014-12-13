-module(core).

-export([app/1,
         bind/1,
         join/1,
         lift/1,
         lift2/1,
         pure/1
        ]).

-include("earsec.hrl").

% Map a function over a parser.
-spec lift(fun((term()) -> term())) -> fun((parser()) -> parser()).
lift(F) ->
    fun(ParserA) ->
        fun(Input) ->
            case ParserA(Input) of
                #state{success = ok, result = A} = State ->
                    State#state{result = F(A)};
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
            ParserBC = (lift(F))(ParserA),
            (app(ParserBC))(ParserB)
        end
    end.

% Lift a term into a trivial parser.
-spec pure(term()) -> parser().
pure(Term) ->
    fun(Input) ->
        #state{success = ok, result = Term, position = 0, remainder = Input}
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
        fun(Input) ->
            case ParserA(Input) of
                #state{success = ok, result = A, position = Position1, remainder = Remainder} ->
                    case (F(A))(Remainder) of
                        #state{success = ok, position = Position2} = State ->
                            State#state{position = Position1 + Position2};
                        Error ->
                            Error#state{position = Position1}
                    end;
                Error ->
                    Error
            end
        end
    end.

-spec join(parser()) -> parser().
join(ParserPA) ->
    (bind(ParserPA))(fun(ParserA) -> ParserA end).
