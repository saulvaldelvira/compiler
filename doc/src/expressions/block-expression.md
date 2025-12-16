
# Block Expressions
r[expr.block.syntax]
```grammar,expression
BlockExpr -> `{` Stmt* Expr? `}`
```
A [BlockExpr] is a block that evaluates to an [expression].
Like any kind of block, it contains a list of [statements][Stmt], and an
optional tail [expression].

[expression]: expr.syntax