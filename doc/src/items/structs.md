# Structs

r[item.struct.syntax]
```grammar,items
StructDecl -> `struct` IDENT `{` Fields? `}`

Fields -> Field (`,` Field)* `,`?

Field -> IDENT `:` Type
```
