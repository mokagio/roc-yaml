module [
    YAML,
    parse
]

YAML := {}

Dummy : { key : Str }

parse : Str -> Dummy
parse = \input ->
    getValueFromKeyValueLine input
    |> Result.map \value -> { key: value }
    |> Result.withDefault { key: "error" }

getValueFromKeyValueLine : Str -> Result Str [ListWasEmpty]
getValueFromKeyValueLine = \input ->
    Str.split input ":"
    |> List.last
    |> Result.map \value -> Str.trim value


expect parse "key: value" == { key: "value" }
