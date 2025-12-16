# If Expressions
r[expr.if.syntax]
```grammar,expression
IfExpr -> `if` Expr BlockExpr `else` BlockExpr
```
An `if` expression evaluates it's conditional [expression].
If it's true, evaluates it's `then` [block]
If it's false, evaluates it's `else` [block]

An if expression -unlike the if statement- _MUST_ have both a
then and an else blocks.

[expression]: expr.syntax
[block]: expr.block.syntax