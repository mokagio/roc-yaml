module [
    YAML,
]

YAML : { key : Str }

parse : Str -> YAML
parse = \_ ->
    { key: "value" }

expect parse "key: value" == { key: "value" }
