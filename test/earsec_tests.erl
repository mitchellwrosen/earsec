-module(earsec_tests).

-include_lib("eunit/include/eunit.hrl").

lift_test() ->
    Parser = earsec:lift(fun(N) -> N+1 end, earsec:uint8()),
    {ok, 2} = earsec:parse(Parser, <<1>>).

lift2_test() ->
    Parser = earsec:lift2(fun(A, B) -> A+B end, earsec:uint8(), earsec:uint8()),
    {ok, 2} = earsec:parse(Parser, <<1, 1>>).

lift3_test() ->
    Parser = earsec:lift3(fun(A, B, C) -> A+B+C end, earsec:uint8(), earsec:uint8(), earsec:uint8()),
    {ok, 3} = earsec:parse(Parser, <<1, 1, 1>>).

lift4_test() ->
    Parser = earsec:lift4(fun(A, B, C, D) -> A+B+C+D end, earsec:uint8(), earsec:uint8(), earsec:uint8(), earsec:uint8()),
    {ok, 4} = earsec:parse(Parser, <<1, 1, 1, 1>>).

lift5_test() ->
    Parser = earsec:lift5(fun(A, B, C, D, E) -> A+B+C+D+E end, earsec:uint8(), earsec:uint8(), earsec:uint8(), earsec:uint8(), earsec:uint8()),
    {ok, 5} = earsec:parse(Parser, <<1, 1, 1, 1, 1>>).

pure_test() ->
    Parser = earsec:pure("foobar"),
    {ok, "foobar"} = earsec:parse(Parser, <<>>).

app_test() ->
    ParserF = earsec:pure(fun(A) -> A+1 end),
    ParserA = earsec:uint8(),
    Parser  = earsec:app(ParserF, ParserA),
    {ok, 2} = earsec:parse(Parser, <<1>>).

empty_test() ->
    Parser = earsec:empty(),
    {error, _} = earsec:parse(Parser, <<>>).

alt_test() ->
    Parser = earsec:alt(earsec:uint16(), earsec:uint8()),
    {ok, 32768} = earsec:parse(Parser, <<32768:16>>),
    {ok, 128} = earsec:parse(Parser, <<128:8>>).
