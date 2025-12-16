r[expr.access]
# Access Expressions

## Field Access
r[expr.access.field.syntax]
```grammar,expression
FieldAccess -> Expr `.` IDENT
```

## Array Access
r[expr.access.array.syntax]
```grammar,expression
ArrayAccess -> Expr `[` Expr `]`
```

## Tuple Access
r[expr.access.tuple.syntax]
```grammar,expression
TupleAccess -> Expr `.` INTEGER_LITERAL
```
