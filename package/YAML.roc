module [
    YAML,
    parse,
]

YAML := {}

KeyValue : { key : Str, value : Str }

parse : Str -> Result KeyValue [ListWasEmpty] # TODO: Use custom error type(s)
parse = \input ->
    getValueFromKeyValueLine input

getValueFromKeyValueLine : Str -> Result KeyValue [ListWasEmpty] # TODO: Add different error for line without :
getValueFromKeyValueLine = \input ->
    if Str.contains input ":" then
        Str.split input ":"
        |> List.last
        |> Result.map \value -> { key: "TODO", value: Str.trim value }
    else
        Err ListWasEmpty

expect parse "key: value" == Ok { key: "TODO", value: "value" }
expect parse "key: other value" == Ok { key: "TODO", value: "other value" }
expect parse "not a YAML" == Err ListWasEmpty
