# Modules

A module is a named [item][Item] that encloses a set [items][Item]

r[item.module.syntax]
```grammar,item
ModuleDecl ->
      `mod` IDENT `{` Item* `}`
    | `extern` `mod` IDENT `;`
```

## Example
```
mod lib {

  mod math {
      fn sum(a: i32, b: i32) -> i32 { a + b }
  }

  mod math_u16 {
    fn sum(a: u16, b: u16) -> u16 { a + b }
  }

  mod person {
      struct Person {
          age: u16,
          name: &char,
      }

      fn is_old(p: &Person) -> bool {
          (*p).age >= 70
      }
  }

}
```