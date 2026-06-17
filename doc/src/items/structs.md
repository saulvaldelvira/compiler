# Structs

r[item.struct.syntax]
```grammar,items
StructDecl -> `struct` IDENT StructBody

StructBody ->
    `{` ( Field (`,` Field)* `,`? )? `}`
    | `;`

Field -> IDENT `:` Type
```
