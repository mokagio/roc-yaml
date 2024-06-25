module [
    UTF8,
    isDigit,
]

## Opaque type for UTF-8 utils
UTF8 := {}

# Originally based on https://github.com/lukewilliamboswell/roc-json/blob/9dd2190872de23cb2d71583bd2a8e65dd4eef46a/package/Json.roc

isDigit : U8 -> Bool
isDigit = \b -> b >= '0' && b <= '9'

expect isDigit '0' == Bool.true
expect isDigit '1' == Bool.true
expect isDigit '8' == Bool.true
expect isDigit '9' == Bool.true
expect isDigit 'a' == Bool.false
expect isDigit '-' == Bool.false
