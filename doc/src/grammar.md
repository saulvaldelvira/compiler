r[program]
# Grammar

A program is composed of a list of [items][Item]
r[program.syntax]
```grammar,item
@root Program -> Item*
```

```grammar,tokens
@root Token ->
    IDENT
    | ALPHANUMERIC
    | FLOAT_LITERAL
    | STRING_LITERAL
    | INTEGER_LITERAL
    | FLOAT_LITERAL

STRING_LITERAL -> `"` (
        ~[`\` `"`]
        | `\` <any char>
    )*? `"`

IDENT -> ALPHABETIC ALPHANUMERIC*

ALPHANUMERIC -> ALPHABETIC | NUMERIC

ALPHABETIC -> [`a`-`z``A`-`Z``_`]

NUMERIC -> [`0`-`9`]

INTEGER_LITERAL -> NUMERIC+

FLOAT_LITERAL -> NUMERIC+ `.` NUMERIC+
```
