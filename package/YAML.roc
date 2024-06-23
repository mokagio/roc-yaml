module [
    YAML,
    parse,
]

YAML := {}

Dummy : { key : Str }

parse : Str -> Result Dummy [ListWasEmpty] # TODO: Use custom error type(s)
parse = \input ->
    getValueFromKeyValueLine input
    |> Result.map \value -> { key: value }

getValueFromKeyValueLine : Str -> Result Str [ListWasEmpty] # TODO: Add different error for line without :
getValueFromKeyValueLine = \input ->
    if Str.contains input ":" then
        Str.split input ":"
        |> List.last
        |> Result.map \value -> Str.trim value
    else
        Err ListWasEmpty

expect parse "key: value" == Ok { key: "value" }
expect parse "key: other value" == Ok { key: "other value" }
expect parse "not a YAML" == Err ListWasEmpty
