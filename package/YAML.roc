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

parse : Str -> Result Node [ListWasEmpty] # TODO: Use custom error type(s)
parse = \input ->
    when List.walkUntil (Str.toUtf8 input) Start parseHelper is
        LookingForColon _ strBytes ->
            when Str.fromUtf8 strBytes is
                Ok valueStr -> Ok (processRawStrIntoValue valueStr)
                Err _ -> Err ListWasEmpty

        LookingForValueEnd _ key valueBytes ->
            when Str.fromUtf8 valueBytes is
                Ok valueStr -> Ok (Map { key, value: processRawStrIntoValue valueStr })
                _ -> Err ListWasEmpty

        LookingForNewLine _ node -> Ok node
        _ -> Err ListWasEmpty

parseHelper : ParsingState, U8 -> [Continue ParsingState, Break ParsingState]
parseHelper = \state, byte ->
    when (state, byte) is
        # FIXME: Indentation matters in multiline YAML! How do we keep track of that?
        (Start, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForFirstNonWhiteSpaceByte (b + 1))

        (Start, b) if UTF8.isAlpha b || UTF8.isDigit b ->
            Continue (LookingForColon (b + 1) [b])

        (Start, b) if '[' == b ->
            Continue (LookingForNextValueInSequence (b + 1) [])

        (LookingForFirstNonWhiteSpaceByte _, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForFirstNonWhiteSpaceByte (b + 1))

        (LookingForColon n tempKey, b) if b != UTF8.colon ->
            Continue (LookingForColon (n + 1) (List.append tempKey b))

        (LookingForColon n tempKey, b) if b == UTF8.colon ->
            when Str.fromUtf8 tempKey is
                Ok key -> Continue (LookingForValue (n + 1) key)
                Err _ -> Break Invalid

        (LookingForValue n key, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForValue (n + 1) key)

        (LookingForValue n key, b) if b == '[' ->
            Continue (LookingForNextValueInMapSequence (n + 1) key [])

        (LookingForValue n key, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.doubleQuote == b || UTF8.singleQuote == b ->
            Continue (LookingForValueEnd (n + 1) key [b])

        (LookingForValueEnd n key tempValue, b) ->
            # TODO: support \n, etc.
            Continue (LookingForValueEnd (n + 1) key (List.append tempValue b))

        (LookingForNextValueInSequence n previousValues, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForNextValueInSequence (n + 1) previousValues)

        (LookingForNextValueInSequence n previousValues, b) if b == ']' ->
            Continue (LookingForNewLine (n + 1) (Sequence previousValues))

        (LookingForNextValueInSequence n previousValues, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.doubleQuote == b || UTF8.singleQuote == b ->
            Continue (LookingForNextValueEndInSequence (n + 1) [b] previousValues)

        (LookingForNextValueEndInSequence n tempValue previousValues, b) if b == ',' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNextValueInSequence (n + 1) (List.append previousValues (processRawStrIntoValue value)))
                Err _ -> Break Invalid

        (LookingForNextValueEndInSequence n tempValue previousValues, b) if UTF8.isAlpha b ->
            Continue (LookingForNextValueEndInSequence (n + 1) (List.append tempValue b) previousValues)

        (LookingForNextValueEndInSequence n tempValue previousValues, b) if b == ']' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNewLine (n + 1) (Sequence (List.append previousValues (processRawStrIntoValue value))))
                Err _ -> Break Invalid

        (LookingForNextValueInMapSequence n key previousValues, b) if UTF8.isWhiteSpace b ->
            Continue (LookingForNextValueInMapSequence (n + 1) key previousValues)

        (LookingForNextValueInMapSequence n key previousValues, b) if UTF8.isAlpha b || UTF8.isDigit b || UTF8.doubleQuote == b || UTF8.singleQuote == b ->
            Continue (LookingForNextValueEndInMapSequence (n + 1) key [b] previousValues)

        (LookingForNextValueEndInMapSequence n key tempValue previousValues, b) if b == ',' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNextValueInMapSequence (n + 1) key (List.append previousValues (processRawStrIntoValue value)))
                Err _ -> Break Invalid

        (LookingForNextValueEndInMapSequence n key tempValue previousValues, b) if b == ']' ->
            when Str.fromUtf8 tempValue is
                Ok value -> Continue (LookingForNewLine (n + 1) (Map { key, value: Sequence (List.append previousValues (processRawStrIntoValue value)) }))
                Err _ -> Break Invalid

        (LookingForNextValueEndInMapSequence n key tempValue previousValues, b) ->
            Continue (LookingForNextValueEndInMapSequence (n + 1) key (List.append tempValue b) previousValues)

        # TODO: How do we handle the end of the input?
        _ ->
            Break Invalid

ParsingState : [
    Start,
    LookingForFirstNonWhiteSpaceByte CurrentByte,
    LookingForKey CurrentByte, # TODO: This works just for the first key, we'll need to add Map or something to support multiple keys
    LookingForColon CurrentByte TempKey, # TODO: This works just for the first key, we'll need to add Map or something to support multiple keys
    LookingForValue CurrentByte Key,
    LookingForValueEnd CurrentByte Key TempValue,
    LookingForNextValueInSequence CurrentByte PreviousValues,
    LookingForNextValueEndInSequence CurrentByte TempValue PreviousValues,
    LookingForNextValueInMapSequence CurrentByte Key PreviousValues,
    LookingForNextValueEndInMapSequence CurrentByte Key TempValue PreviousValues,
    LookingForNewLine CurrentByte Node, # TODO: This only work if there was a full node on the line...
    Invalid,
]

CurrentByte : U8
TempKey : List U8
TempValue : List U8
PreviousValues : List Value

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
expect parse "not a YAML" == Ok (Scalar (String "not a YAML"))
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
