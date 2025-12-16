# Statements

r[stmt.syntax]
```grammar,statements
Stmt -> Item
      IfStmt
    | WhileStmt
    | BlockStmt
    | Expr `;`
    | `break`
    | `continue`
    | `return`

WhileStmt -> `while` `(` Expr `)` Stmt

IfStmt -> `if` Expr BlockStmt (`else` BlockStmt)?

BlockStmt -> `{` Stmt* `}`
```
