module [
    YAML,
]

YAML := {}

Dummy : { key : Str }

parse : Str -> Dummy
parse = \_ ->
    { key: "value" }

expect parse "key: value" == { key: "value" }
