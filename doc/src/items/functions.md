# Functions

r[item.function.syntax]
```grammar,items
FunctionDecl ->
      `fn` IDENT `(` ParamList? `)` (`->` Type)?  BlockExpr
    | `extern` `fn` IDENT `(` ParamList? `)` (`->` Type)?  `;`

Parameter -> IDENT `:` Type

ParamList -> Parameter (`,` Parameter )*
```