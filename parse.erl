-module(parse).

-compile(export_all).

% type Parser a = Binary -> Either Error (a, Binary)
-type parser() :: fun((binary()) -> {ok, term(), binary()} | error).

% lift :: (a -> b) -> Parser a -> Parser b
-spec lift(fun((term()) -> term())) -> fun((parser()) -> parser()).
lift(F) ->
    fun(ParserA) ->
        fun(Binary) ->
            case ParserA(Binary) of
                {ok, A, Binary2} ->
                    {ok, F(A), Binary2};
                error ->
                    error
            end
        end
    end.

% lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-spec lift2(fun((term(), term()) -> term())) -> fun((parser()) -> fun((parser()) -> parser())).
lift2(F) ->
    fun(ParserA) ->
        fun(ParserB) ->
            ParserBC = (lift(F))(ParserA),
            (app(ParserBC))(ParserB)
        end
    end.

% pure :: a -> Parser a
-spec pure(term()) -> parser().
pure(Term) ->
    fun(Binary) ->
        {ok, Term, Binary}
    end.

% app :: Parser (a -> b) -> Parser a -> Parser b
-spec app(parser()) -> fun((parser()) -> parser()).
app(ParserF) ->
    fun(ParserA) ->
        fun(Binary) ->
            case ParserF(Binary) of
                {ok, F, Binary2} ->
                    ParserB = (lift(F))(ParserA),
                    ParserB(Binary2);
                error ->
                    error
            end
        end
    end.

% bind :: Parser a -> (a -> Parser b) -> Parser b
-spec bind(parser()) -> fun((fun((term()) -> parser())) -> parser()).
bind(ParserA) ->
    fun(F) ->
        fun(Binary) ->
            case ParserA(Binary) of
                {ok, A, Binary2} ->
                    (F(A))(Binary2);
                error ->
                    error
            end
        end
    end.

% join :: Parser (Parser a) -> Parser a
-spec join(parser()) -> parser().
join(ParserPA) ->
    (bind(ParserPA))(fun(ParserA) -> ParserA end).

-spec int8() -> parser().
int8() ->
    fun(Binary) ->
        case Binary of
            <<N:8, Rest/binary>> ->
                {ok, N, Rest};
            _ ->
                error
        end
    end.

-spec two_int8s() -> parser().
two_int8s() ->
    F = fun(A, B) -> {A, B} end,
    ((lift2(F))(int8/0))(int8/0).
