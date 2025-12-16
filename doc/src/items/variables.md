# Variables

r[item.let.syntax]
```grammar,items
VarDecl ->
      `let` (`:` Type)? IDENT (`=` Expr)? `;`
    | `const` `:` Type IDENT `=` Expr `;`
```