# Call Expressions

r[expr.call.syntax]
```grammar,expression
CallExpr -> Expr `(` Args? `)`

Args -> Expr ( `,` Expr )*
```