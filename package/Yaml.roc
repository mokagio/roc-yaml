module [
    Yaml,
]

Yaml : { key : Str }

parse : Str -> Yaml
parse = \_ ->
    { key: "value" }

expect parse "key: value" == { key: "value" }
