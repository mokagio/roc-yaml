module [
    YAML,
    parse,
]

YAML := {}

Dummy : { key : Str }

parse : Str -> Dummy
parse = \input ->
    getValueFromKeyValueLine input
    |> Result.map \value -> { key: value }
    |> Result.withDefault { key: "error" }

getValueFromKeyValueLine : Str -> Result Str [ListWasEmpty] # TODO: Add different error for line without :
getValueFromKeyValueLine = \input ->
    if Str.contains input ":" then
        Str.split input ":"
        |> List.last
        |> Result.map \value -> Str.trim value
    else
        Err ListWasEmpty

expect parse "key: value" == { key: "value" }
expect parse "key: other value" == { key: "other value" }
expect parse "not a YAML" == { key: "error" }
