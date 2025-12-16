r[expr]
# Expressions

Expressions are the main syntactical construction of the language.
They represent an operation that yields a value.

r[expr.syntax]
```grammar,expression
Expr ->
      BlockExpr
    | ArithmeticExpr
    | LogicalExpr
    | CallExpr
    | IfExpr
    | Path
    | LiteralExpr
    | ReferenceExpr
    | DereferenceExpr
    | FieldAccess
    | ArrayAccess
    | TupleAccess
    | `(` Expr `)`
```
