module [
    UTF8,
    singleQuote,
    doubleQuote,
    isDigit,
    isWhiteSpace,
]

## Opaque type for UTF-8 utils
UTF8 := {}

singleQuote = '\''
doubleQuote = '"'

# Originally based on https://github.com/lukewilliamboswell/roc-json/blob/9dd2190872de23cb2d71583bd2a8e65dd4eef46a/package/Json.roc

isDigit : U8 -> Bool
isDigit = \b -> b >= '0' && b <= '9'

expect isDigit '0' == Bool.true
expect isDigit '1' == Bool.true
expect isDigit '8' == Bool.true
expect isDigit '9' == Bool.true
expect isDigit 'a' == Bool.false
expect isDigit '-' == Bool.false

# TODO: There might be more of these
isWhiteSpace : U8 -> Bool
isWhiteSpace = \byte ->
    when byte is
        ' ' | '\t' | '\n' | '\r' -> Bool.true # space (32), tab (9), newline (10), carriage return (13)
        _ -> Bool.false

expect isWhiteSpace ' ' == Bool.true
expect isWhiteSpace 'a' == Bool.false
expect isWhiteSpace '1' == Bool.false
expect isWhiteSpace '-' == Bool.false
expect isWhiteSpace '\t' == Bool.true
expect isWhiteSpace '\n' == Bool.true
expect isWhiteSpace '\r' == Bool.true
