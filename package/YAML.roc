module [
    YAML,
    parse,
]

import UTF8

## https://yaml.org/spec/1.1/current.html
YAML := {}

Node : [
    Scalar Scalar,
    Sequence Sequence,
    Map Map,
]

## The content of a scalar node is an opaque datum that can be presented as a series of zero or more Unicode characters.
##
## TODO: This means that we might need to change the type to be a bag of characters? But then, how to get a Str, Dec, etc. out?
##
## https://yaml.org/spec/1.1/current.html#scalar/information%20model
Scalar : [
    String Str,
    Decimal Dec,
    Boolean Bool,
    # TODO: What else?
]

## The content of a mapping node is an unordered set of key: value node pairs, with the restriction that each of the keys is unique.
## YAML places no further restrictions on the nodes.
## In particular, keys may be arbitrary nodes, the same node may be used as the value of several key: value pairs, and a mapping could even contain itself as a key or a value (directly or indirectly).
##
## https://yaml.org/spec/1.1/current.html#mapping/information%20model
Map : { key : Key, value : Value }

## https://yaml.org/spec/1.1/current.html#sequence/information%20model
Sequence : List Value

Value : [
    Scalar Scalar,
    Sequence (List Value), # can't use Sequence Sequence because it would be self-recursive—roc_lang 2024-06-18-41ea2bfbc7d
    Map { key : Key, value : Value },
]

Key : Str

parse : Str -> Result Node [ParsingFailed]
parse = \input -> parseBytes (Str.toUtf8 input)

parseBytes : List U8 -> Result Node [ParsingFailed]
parseBytes = \input ->
    when List.walkUntil input Start parseHelper is
        Accumulating _ candidate ->
            when candidate is
                ScalarOrMapKey bytes ->
                    when Str.fromUtf8 bytes is
                        Ok rawScalar -> Ok (processRawStrIntoValue rawScalar)
                        Err _ -> Err ParsingFailed

                MapValue bytes key ->
                    when Str.fromUtf8 bytes is
                        Ok rawValue -> Ok (Map { key, value: processRawStrIntoValue rawValue })
                        Err _ -> Err ParsingFailed

                SequenceValue bytes previousValues ->
                    when Str.fromUtf8 bytes is
                        Ok value -> Ok (Sequence (List.map (List.append previousValues value) processRawStrIntoValue))
                        Err _ -> Err ParsingFailed

        LookingForNewLine _ node -> Ok node
        _ -> Err ParsingFailed

parseHelper : ParsingState, U8 -> [Continue ParsingState, Break ParsingState]
parseHelper = \state, byte ->
    when (state, byte) is
        # FIXME: Indentation matters in multiline YAML! How do we keep track of that?
        (Start, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForFirstNonWhiteSpaceByte (b + 1))

        (Start, b) if UTF8.isAlpha b || UTF8.isDigit b ->
            Continue (Accumulating (b + 1) (ScalarOrMapKey [b]))

        (Start, b) if '[' == b ->
            # Continue (LookingForNextValueInSequence (b + 1) [])
            Continue (Accumulating (b + 1) (SequenceValue [] []))

        # FIXME: Better check needed, can't use == '_' etc.
        (Accumulating n candidate, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.isWhiteSpace b || b == '_' || b == '"' || b == '\'' ->
            when candidate is
                ScalarOrMapKey bytes -> Continue (Accumulating (n + 1) (ScalarOrMapKey (List.append bytes b)))
                MapValue bytes key -> Continue (Accumulating (n + 1) (MapValue (List.append bytes b) key))
                SequenceValue bytes previousValues -> Continue (Accumulating (n + 1) (SequenceValue (List.append bytes b) previousValues))

        (Accumulating n candidate, b) if b == ',' ->
            when candidate is
                ScalarOrMapKey bytes -> Continue (Accumulating (n + 1) (ScalarOrMapKey (List.append bytes b)))
                MapValue bytes key -> Continue (Accumulating (n + 1) (MapValue (List.append bytes b) key))
                SequenceValue bytes previousValues ->
                    when Str.fromUtf8 bytes is
                        Ok value -> Continue (Accumulating (n + 1) (SequenceValue [] (List.append previousValues value)))
                        Err _ -> Break Invalid

        (Accumulating n candidate, b) if b == UTF8.colon ->
            when candidate is
                ScalarOrMapKey bytes ->
                    when Str.fromUtf8 bytes is
                        Ok key -> Continue (Accumulating (n + 1) (MapValue [] (Str.trimEnd key)))
                        Err _ -> Break Invalid

                _ -> Break Invalid

        (Accumulating n candidate, b) if b == '[' || b == ']' ->
            when candidate is
                # FIXME: "ab[" is actually a valid YAML scalar...
                ScalarOrMapKey _ -> Break Invalid
                MapValue bytes key -> Continue (Accumulating (n + 1) (MapValue (List.append bytes b) key))
                SequenceValue bytes previousValues ->
                    if b == ']' then
                        if List.isEmpty bytes then
                            Continue (LookingForNewLine (n + 1) (Sequence (List.map previousValues processRawStrIntoValue)))
                        else
                            when Str.fromUtf8 bytes is
                                Ok rawValue ->
                                    Continue (LookingForNewLine (n + 1) (Sequence (List.map (List.append previousValues rawValue) processRawStrIntoValue)))

                                Err _ ->
                                    Break Invalid
                    else
                        Break Invalid

        (LookingForFirstNonWhiteSpaceByte _, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForFirstNonWhiteSpaceByte (b + 1))

        # TODO: How do we handle the end of the input?
        _ ->
            Break Invalid

ParsingState : [
    Start,
    LookingForFirstNonWhiteSpaceByte U8,
    Accumulating U8 Candidate,
    LookingForNewLine U8 Node, # TODO: This only work if there was a full node on the line...
    Invalid,
]

Candidate : [
    ScalarOrMapKey (List U8),
    MapValue (List U8) Key,
    SequenceValue (List U8) (List Str),
]

# TODO: Rewrite this as a walkUntil parser, too
processRawStrIntoValue : Str -> Value
processRawStrIntoValue = \rawStr ->
    trimmed = Str.trim rawStr # FIXME: Indentation matters in YAML

    # If the string is wrapped in quotes, then it can't be anything other than a string
    if isWrappedInDoubleQuotes trimmed then
        Scalar (String (stripDoubleQuotes trimmed))
    else if isWrappedInSingleQuotes trimmed then
        Scalar (String (stripSingleQuotes trimmed))
    else if isWrappedIn trimmed '[' ']' then
        Sequence
            (
                Str.split (Str.replaceFirst (Str.replaceLast trimmed "]" "") "[" "") ","
                |> List.map processRawStrIntoValue
            )
    else if isInteger trimmed then
        when Str.toDec trimmed is
            Ok value -> Scalar (Decimal value)
            Err _ -> Scalar (String "failed to decode number")
    else
        when toBool trimmed is
            Ok value -> Scalar (Boolean value)
            Err _ -> Scalar (String trimmed)

isInteger : Str -> Bool
isInteger = \str ->
    trimmed = Str.trim str

    Str.walkUtf8 trimmed Bool.true \answer, byte -> answer && UTF8.isDigit byte == Bool.true

expect isInteger "123" == Bool.true
expect isInteger "abc" == Bool.false

toBool : Str -> Result Bool [NotABoolean]
toBool = \str ->
    if str == "true" then
        Ok Bool.true
    else if str == "false" then
        Ok Bool.false
    else
        Err NotABoolean

expect toBool "true" == Ok Bool.true
expect toBool "false" == Ok Bool.false
expect toBool "abc" == Err NotABoolean
expect toBool "tru" == Err NotABoolean
expect toBool "rue" == Err NotABoolean
expect toBool " true" == Err NotABoolean
expect toBool "false " == Err NotABoolean

isWrappedInSingleQuotes : Str -> Bool
isWrappedInSingleQuotes = \str -> isWrappedIn str UTF8.singleQuote UTF8.singleQuote

expect isWrappedInSingleQuotes "\"abc\"" == Bool.false
expect isWrappedInSingleQuotes "\"abc'" == Bool.false
expect isWrappedInSingleQuotes "'abc\"" == Bool.false
expect isWrappedInSingleQuotes "'abc'" == Bool.true
expect isWrappedInSingleQuotes "abc" == Bool.false

isWrappedInDoubleQuotes : Str -> Bool
isWrappedInDoubleQuotes = \str -> isWrappedIn str UTF8.doubleQuote UTF8.doubleQuote

expect isWrappedInDoubleQuotes "\"abc\"" == Bool.true
expect isWrappedInDoubleQuotes "\"abc'" == Bool.false
expect isWrappedInDoubleQuotes "'abc\"" == Bool.false
expect isWrappedInDoubleQuotes "'abc'" == Bool.false
expect isWrappedInDoubleQuotes "abc" == Bool.false

isWrappedIn : Str, U8, U8 -> Bool
isWrappedIn = \str, startWrapper, endWrapper ->
    when List.walkUntil (Str.toUtf8 str) Start (isWrappedInHelper startWrapper endWrapper) is
        FoundBothWrappers _ -> Bool.true
        _ -> Bool.false

expect isWrappedIn "\"abc\"" '"' '"' == Bool.true
expect isWrappedIn "'abc'" '"' '"' == Bool.false
expect isWrappedIn "'abc'" '\'' '\'' == Bool.true
expect isWrappedIn "[abc'" '\'' '\'' == Bool.false
expect isWrappedIn "[abc'" '[' '\'' == Bool.true
expect isWrappedIn " 'abc'" '\'' '\'' == Bool.true
expect isWrappedIn "'abc' " '\'' '\'' == Bool.true
expect isWrappedIn "'abc'   " '\'' '\'' == Bool.true
expect isWrappedIn "   'abc'" '\'' '\'' == Bool.true
expect isWrappedIn "  'abc'   " '\'' '\'' == Bool.true

isWrappedInHelper : U8, U8 -> (WrappedSearchState, U8 -> [Continue WrappedSearchState, Break WrappedSearchState])
isWrappedInHelper = \startWrapper, endWrapper ->
    \state, byte ->
        when (state, byte) is
            (Start, b) if b != startWrapper -> Continue (LookingForStartWrapper (b + 1))
            (Start, b) if b == startWrapper -> Continue (LookingForEndWrapper (b + 1))
            (LookingForStartWrapper n, b) if b != startWrapper -> Continue (LookingForStartWrapper (n + 1))
            (LookingForStartWrapper n, b) if b == startWrapper -> Continue (LookingForEndWrapper (n + 1))
            (LookingForEndWrapper n, b) if b != endWrapper -> Continue (LookingForEndWrapper (n + 1))
            (LookingForEndWrapper n, b) if b == endWrapper -> Break (FoundBothWrappers n)
            _ -> Break Invalid

WrappedSearchState : [
    Start,
    LookingForStartWrapper U8,
    LookingForEndWrapper U8,
    FoundBothWrappers U8,
    Invalid,
]

stripDoubleQuotes : Str -> Str
stripDoubleQuotes = \str ->
    Str.replaceFirst str doubleQuote ""
    |> Str.replaceLast doubleQuote ""

expect stripDoubleQuotes "\"abc\"" == "abc"
expect stripDoubleQuotes "abc" == "abc"
expect stripSingleQuotes "abc" == "abc"
# FIXME: Decide whether this is the desired behavior? It will do for now while the behavior is unrefined
expect stripDoubleQuotes "abc\"" == "abc"
expect stripDoubleQuotes "abc\"\"" == "abc"
expect stripDoubleQuotes "\"\"abc\"" == "\"abc"

stripSingleQuotes : Str -> Str
stripSingleQuotes = \str ->
    Str.replaceFirst str singleQuote ""
    |> Str.replaceLast singleQuote ""

expect stripSingleQuotes "'abc'" == "abc"
expect stripSingleQuotes "abc" == "abc"
# FIXME: Decide whether this is the desired behavior? It will do for now while the behavior is unrefined
expect stripSingleQuotes "abc'" == "abc"
expect stripSingleQuotes "abc''" == "abc"
expect stripSingleQuotes "''abc'" == "'abc"

expect parse "key: value" == Ok (Map { key: "key", value: Scalar (String "value") })
expect parse "key: other value" == Ok (Map { key: "key", value: Scalar (String "other value") })
expect parse "other_key: yet other value" == Ok (Map { key: "other_key", value: Scalar (String "yet other value") })
expect parse "key: 1" == Ok (Map { key: "key", value: Scalar (Decimal 1) })
expect parse "key: true" == Ok (Map { key: "key", value: Scalar (Boolean Bool.true) })
expect parse "k: false" == Ok (Map { key: "k", value: Scalar (Boolean Bool.false) })
expect parse "k:    false" == Ok (Map { key: "k", value: Scalar (Boolean Bool.false) })
expect parse "k: false " == Ok (Map { key: "k", value: Scalar (Boolean Bool.false) })
expect parse "k: fa lse " == Ok (Map { key: "k", value: Scalar (String "fa lse") })
expect parse "key: \"true\"" == Ok (Map { key: "key", value: Scalar (String "true") })
expect parse "key: 'true'" == Ok (Map { key: "key", value: Scalar (String "true") })
expect parse "key: [1,2]" == Ok (Map { key: "key", value: Sequence [Scalar (Decimal 1), Scalar (Decimal 2)] })
expect parse "key: [1]" == Ok (Map { key: "key", value: Sequence [Scalar (Decimal 1)] })
expect parse "key: [a, b]" == Ok (Map { key: "key", value: Sequence [Scalar (String "a"), Scalar (String "b")] })
expect parse "key: [b, 1]" == Ok (Map { key: "key", value: Sequence [Scalar (String "b"), Scalar (Decimal 1)] })
expect parse "key: [ab, c]" == Ok (Map { key: "key", value: Sequence [Scalar (String "ab"), Scalar (String "c")] })
expect parse "a standalone string is still YAML" == Ok (Scalar (String "a standalone string is still YAML"))
expect parse "1" == Ok (Scalar (Decimal 1))
expect parse "true" == Ok (Scalar (Boolean Bool.true))
expect parse "false" == Ok (Scalar (Boolean Bool.false))
expect parse "f al  se" == Ok (Scalar (String "f al  se"))
# TODO: Nested lists
# expect parse "key: [c, [1,2]]" == Ok (Map { key: "key", value: Sequence [String "c", Sequence [Decimal 1, Decimal 2]] })
expect parse "[1,2]" == Ok (Sequence [Scalar (Decimal 1), Scalar (Decimal 2)])
expect parse "[1]" == Ok (Sequence [Scalar (Decimal 1)])
expect parse "[]" == Ok (Sequence [])
expect parse "[1, 2, 3, 4]" == Ok (Sequence [Scalar (Decimal 1), Scalar (Decimal 2), Scalar (Decimal 3), Scalar (Decimal 4)])
expect parse "[ab]" == Ok (Sequence [Scalar (String "ab")])
expect parse "[ab, cde]" == Ok (Sequence [Scalar (String "ab"), Scalar (String "cde")])
expect parse "[a, 1, true]" == Ok (Sequence [Scalar (String "a"), Scalar (Decimal 1), Scalar (Boolean Bool.true)])

singleQuote = "'"
doubleQuote = "\""
