module [
    YAML,
    parse,
]

YAML := {}

KeyValue : { key : Str, value : Value }

# Value : [
#     Str,
#     Num, # FIXME Possibly not the right type here. Might need more refined Int, Float, etc.
# ]
Value : Str

parse : Str -> Result KeyValue [ListWasEmpty] # TODO: Use custom error type(s)
parse = \input ->
    when getKeyFromKeyValueLine input is
        Ok key ->
            when getValueFromKeyValueLine input is
                Ok value -> Ok { key, value }
                _ -> Err ListWasEmpty

        _ -> Err ListWasEmpty

# get key from value from line with key: value
getKeyFromKeyValueLine : Str -> Result Value [ListWasEmpty] # TODO: Add different error for line without :
getKeyFromKeyValueLine = \input ->
    if Str.contains input colon then
        Str.split input colon
        |> List.first
        |> Result.map processRawStrIntoValue # TODO indentaion matters in YAML
    else
        Err ListWasEmpty

processRawStrIntoValue : Str -> Value
processRawStrIntoValue = \rawStr ->
    # TODO: Value is currently [Str] only
    Str.trim rawStr

isDigit : U8 -> Bool
isDigit = \b -> b >= '0' && b <= '9'

expect Str.walkUtf8 "123" Bool.true \answer, byte -> answer && isDigit byte == Bool.true
expect Str.walkUtf8 "abc" Bool.true \answer, byte -> answer && isDigit byte == Bool.false

expect isDigit '0' == Bool.true
expect isDigit '1' == Bool.true
expect isDigit '8' == Bool.true
expect isDigit '9' == Bool.true
expect isDigit 'a' == Bool.false
expect isDigit '-' == Bool.false

getValueFromKeyValueLine : Str -> Result Str [ListWasEmpty] # TODO: Add different error for line without :
getValueFromKeyValueLine = \input ->
    if Str.contains input colon then
        Str.split input colon
        |> List.last
        |> Result.map Str.trim
    else
        Err ListWasEmpty

colon = ":"

expect parse "key: value" == Ok { key: "key", value: "value" }
expect parse "key: other value" == Ok { key: "key", value: "other value" }
expect parse "other_key: yet other value" == Ok { key: "other_key", value: "yet other value" }
# expect parse "key: 1" == Ok { key: "key", value: 1 }
expect parse "not a YAML" == Err ListWasEmpty
