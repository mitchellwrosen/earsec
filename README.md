If you are coming from an Erlang background you may be unaware of what a parser
combinator library even *is*. We start with a core data type

```erlang
-type parser() :: fun((binary()) -> {ok, {term(), integer(), binary()}}
                                  | {error, {term(), integer(), binary()}}
```

and attempt to build large parsers from reusable building-block parsers.

Consider parsing a simple binary message format like this one, from Kafka:

    ProduceResponse => [TopicName [Partition ErrorCode Offset]]

where

    TopicName => string
    Partition => int32
    ErrorCode => int16
    Offset => int64

Here, a term is denoted by a word beginning with a capital letter. A space
between two terms indicates adjacent terms. Braces signify an `array`, which
is preceded by the number of items in the array in a 32 bit (big-endian)
integer. A `string` is preceded by an int16, indicating its length.

Let's start by writing a combinator that modifies an arbitrary term *parser*
to parse a *Kafka array* of terms. That is, whatever the parser did before,
spit out a new parser that first parses a 32-bit integer, then applies the
original parser to the input that many times.

```erlang
-spec kafka_array(parser()) -> parser().
kafka_array(Parser) ->
    earsec:bind(earsec:uint32(), fun(N) ->
        earsec:count(N, Parser)
    end).
```

Next, let's write a parser for `Partition ErrorCode Offset` called `partition_info`.
Though it's not expressed in the type, this parser returns terms of the form
`{int32(), int16(), int64()}`.

```erlang
-spec partition_info() -> parser().
partition_info() ->
    earsec:lift3(fun(Partition, ErrorCode, Offset) ->
                     {Partition, ErrorCode, Offset}
                 end,
                 earsec:uint32(),
                 earsec:uint16(),
                 earsec:uint64()).
```

Next, let's use these two parsers to write a parser for `[Parser ErrorCode Offset]`.

```erlang
-spec partition_info_array() -> parser().
partition_info_array() ->
    kafka_array(partition_info()).
```

Next, let's write a parser for `Topic [Partition ErrorCode Offset]`. It looks like
`Topic` is a Kafka `string`, so let's first write a parser for that. Remember,
it's simply binary data preceded by 16 bits indicating its length.

```erlang
-spec kafka_string() -> parser().
kafka_string() ->
    earsec:bind(earsec:uint16(), fun(Len) ->
        earsec:binary(Len)
    end).
```

Now, back to parsing `Topic [Partition ErrorCode Offset]`. This parser will return
a term of the following shape: `{Topic, [PartitionInfo]}`.

```erlang
-spec topic_info() -> parser().
topic_info() ->
    earsec:lift2(fun(Topic, PartitionInfoArray) ->
                     {Topic, PartitionInfoArray}
                 end,
                 kafka_string(),
                 partition_info_array()).
```

And finally, an array of them:

```erlang
-spec produce_response() -> parser().
produce_response() ->
    kafka_array(topic_info()).
```

Neat-o!
