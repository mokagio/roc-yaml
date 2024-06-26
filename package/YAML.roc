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
    String Str,
    Decimal Dec,
    Boolean Bool,
    Sequence (List Value), # can't use Sequence Sequence because of it's self-recursive
]

parse : Str -> Result Node [ListWasEmpty] # TODO: Use custom error type(s)
parse = \input ->
    when List.walkUntil (Str.toUtf8 input) Start parseHelper is
        LookingForValueEnd _ key valueBytes ->
            when Str.fromUtf8 valueBytes is
                Ok valueStr -> Ok (Map { key, value: processRawStrIntoValue valueStr })
                _ -> Err ListWasEmpty

        LookingForNewLine _ key value -> Ok (Map { key, value })
        _ -> Err ListWasEmpty

processRawStrIntoValue : Str -> Value
processRawStrIntoValue = \rawStr ->
    trimmed = Str.trim rawStr # FIXME: Indentation matters in YAML

    # If the string is wrapped in quotes, then it can't be anything other than a string
    if isWrappedInDoubleQuotes trimmed then
        String (stripDoubleQuotes trimmed)
    else if isWrappedInSingleQuotes trimmed then
        String (stripSingleQuotes trimmed)
    else if isWrappedIn trimmed '[' ']' then
        Sequence
            (
                Str.split (Str.replaceFirst (Str.replaceLast trimmed "]" "") "[" "") ","
                |> List.map processRawStrIntoValue
            )
    else if isInteger trimmed then
        when Str.toDec trimmed is
            Ok value -> Decimal value
            Err _ -> String "failed to decode number"
    else
        when toBool trimmed is
            Ok value -> Boolean value
            Err _ -> String trimmed

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

parseHelper : ParsingState, U8 -> [Continue ParsingState, Break ParsingState]
parseHelper = \state, byte ->
    when (state, byte) is
        (Start, b) if UTF8.isWhiteSpace b -> Continue (LookingForKey (b + 1))
        (Start, b) if UTF8.isAlpha b || UTF8.isDigit b -> Continue (LookingForColon (b + 1) [b])
        (LookingForColon n tempKey, b) if b != UTF8.colon -> Continue (LookingForColon (n + 1) (List.append tempKey b))
        (LookingForColon n tempKey, b) if b == UTF8.colon ->
            when Str.fromUtf8 tempKey is
                Ok key -> Continue (LookingForValue (n + 1) key)
                Err _ -> Break Invalid

        (LookingForValue n key, b) if UTF8.isWhiteSpace b -> Continue (LookingForValue (n + 1) key)
        (LookingForValue n key, b) if b == '[' -> Continue (LookingForNextValueInSequence (n + 1) key [])
        (LookingForValue n key, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.doubleQuote == b || UTF8.singleQuote == b ->
            Continue (LookingForValueEnd (n + 1) key [b])

        (LookingForValueEnd n key tempValue, b) ->
            # TODO: support \n, etc.
            Continue (LookingForValueEnd (n + 1) key (List.append tempValue b))

        (LookingForNextValueInSequence n key previousValues, b) if UTF8.isWhiteSpace b -> Continue (LookingForNextValueInSequence (n + 1) key previousValues)
        (LookingForNextValueInSequence n key previousValues, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.doubleQuote == b || UTF8.singleQuote == b ->
            Continue (LookingForNextValueEndInSequence (n + 1) key [b] previousValues)

        (LookingForNextValueEndInSequence n key tempValue previousValues, b) if b == ',' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNextValueInSequence (n + 1) key (List.append previousValues (processRawStrIntoValue value)))
                Err _ -> Break Invalid

        (LookingForNextValueEndInSequence n key tempValue previousValues, b) if b == ']' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNewLine (n + 1) key (Sequence (List.append previousValues (processRawStrIntoValue value))))
                Err _ -> Break Invalid

        (LookingForNextValueEndInSequence n key tempValue previousValues, b) ->
            Continue (LookingForNextValueEndInSequence (n + 1) key (List.append tempValue b) previousValues)

        # TODO: How do we handle the end of the input?
        _ ->
            Break Invalid

ParsingState : [
    Start,
    LookingForKey CurrentByte, # TODO: This works just for the first key, we'll need to add Map or something to support multiple keys
    LookingForColon CurrentByte TempKey, # TODO: This works just for the first key, we'll need to add Map or something to support multiple keys
    LookingForValue CurrentByte Key,
    LookingForValueEnd CurrentByte Key TempValue,
    LookingForNextValueInSequence CurrentByte Key PreviousValues,
    LookingForNextValueEndInSequence CurrentByte Key TempValue PreviousValues,
    LookingForNewLine CurrentByte Key Value,
    Invalid,
]

CurrentByte : U8
TempKey : List U8
Key : Str
TempValue : List U8
PreviousValues : List Value

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

expect parse "key: value" == Ok (Map { key: "key", value: String "value" })
expect parse "key: other value" == Ok (Map { key: "key", value: String "other value" })
expect parse "other_key: yet other value" == Ok (Map { key: "other_key", value: String "yet other value" })
expect parse "key: 1" == Ok (Map { key: "key", value: Decimal 1 })
expect parse "key: true" == Ok (Map { key: "key", value: Boolean Bool.true })
expect parse "k: false" == Ok (Map { key: "k", value: Boolean Bool.false })
expect parse "k:    false" == Ok (Map { key: "k", value: Boolean Bool.false })
expect parse "k: false " == Ok (Map { key: "k", value: Boolean Bool.false })
expect parse "k: fa lse " == Ok (Map { key: "k", value: String "fa lse" })
expect parse "key: \"true\"" == Ok (Map { key: "key", value: String "true" })
expect parse "key: 'true'" == Ok (Map { key: "key", value: String "true" })
expect parse "key: [1,2]" == Ok (Map { key: "key", value: Sequence [Decimal 1, Decimal 2] })
expect parse "key: [1]" == Ok (Map { key: "key", value: Sequence [Decimal 1] })
expect parse "key: [a, b]" == Ok (Map { key: "key", value: Sequence [String "a", String "b"] })
expect parse "key: [b, 1]" == Ok (Map { key: "key", value: Sequence [String "b", Decimal 1] })
# TODO: Nested lists
# expect parse "key: [c, [1,2]]" == Ok (Map { key: "key", value: Sequence [String "c", Sequence [Decimal 1, Decimal 2]] })
expect parse "not a YAML" == Err ListWasEmpty

singleQuote = "'"
doubleQuote = "\""
