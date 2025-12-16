# Types

r[type.syntax]
```grammar,types
Type ->
      `i32` | `i16` | `i32` | `i64`
    | `u8`  | `u16` | `u32` | `u64`
    | `f32` | `f64`
    | `char`
    | `bool`
    | Path
    | `[` Type `;` INTEGER_LITERAL ` ]`
    | `(` TypeList? `)`
    | `&` Type
    | `&&` Type

TypeList -> Type ( `,` Type )*
```

# Example
```
let a: i32;

struct Person {
    age: u16,
    name: &char,
}

let people: [Person, 12];
```
