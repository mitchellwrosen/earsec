-module(earsec).

-export([make_parser/1,
         parse/2
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
                    {Error, Position1, Remainder1}, undefined}
            end;
        (Error) ->
            Error
    end.
