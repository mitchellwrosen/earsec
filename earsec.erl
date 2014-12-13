-module(earsec).

-export([parse/2]).

-include("earsec.hrl").

% Run a parser over input, succeeding only if the parser succeeds, and there is no trailing input.
%
% Returns: 
%   {ok, Result} on success
%   {error, {trailing_input, Position, Remainder}} on successful parse with trailing input
%   {error, {parse_error, FailedParser, Position, Remainder} on failed parse
-spec parse(parser(), binary()) -> {ok, term()} | {error, term()}.
parse(Parser, Input) ->
    case Parser(Input) of
        #state{success = ok, result = Result, remainder = <<>>} ->
            {ok, Result};
        #state{success = ok, position = Position, remainder = Remainder} ->
            {error, {trailing_input, Position, Remainder}};
        #state{success = {error, FailedParser}, position = Position, remainder = Remainder} ->
            {error, {parse_error, FailedParser, Position, Remainder}}
    end.
