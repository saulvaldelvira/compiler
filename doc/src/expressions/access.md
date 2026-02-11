r[expr.access]
# Access Expressions

## Field Access
r[expr.access.field.syntax]
```grammar,expression
FieldAccess -> Expr `.` IDENT
```

r[expr.access.field.autoderef]
### Auto-deref semantics
A field access on an expression of type 'pointer to struct' is
semantically equivalent as if an explicit dereference on such expression
was performed prior to the access.

Example:
```
struct Person {
    age: i32,
    name: &char,
}

let p: Person;
let ptr: &Person = &p;

ptr.age = 12;
(*ptr).age = 12;
// ^ These two are the same
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
