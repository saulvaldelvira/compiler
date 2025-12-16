# Operators

## Arithmetic expressions
r[expr.arithmetic.syntax]
```grammar,expression
ArithmeticExpr ->
      Expr [`*` `/` `%`] Expr
    | Expr [`+` `-`] Expr
```

## Logical Expressions
r[expr.logical.syntax]
```grammar,expression
LogicalExpr ->
      Expr `&&` Expr
    | Expr `||` Expr
```

## (De)Reference expressions

r[expr.ref.syntax]
```grammar,expression
ReferenceExpr ->
      `&` Expr
    | `&&` Expr

DereferenceExpr -> `*` Expr
```
