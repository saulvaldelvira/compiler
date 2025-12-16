# Use Items

`use` items allow to bring other items into scope.

r[item.use.syntax]
```grammar,items
UseItem ->
    `use` Type `as` IDENT `;`
    | `use` Path (`as` IDENT)? `;`
    | `use` Path `::` `*` `;`
```

# Example
```
mod utils {
    fn max(a: i32, b: i32) -> i32 {
        if a > b { a } else { b }
    }

    fn abs(a: i32) -> i32 {
        if a > 0 { a } else { a * -1 }
    }
}

use utils::max;
use utils as very_util;

fn main() {
    let a: i32 = max(1, 2);

    let b: i32 = very_util::abs(-3);
}
```