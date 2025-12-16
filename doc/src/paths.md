# Paths
A path is a way of refering to named [items][Item].

r[path.syntax]
```grammar,path
PathPrefix -> (`self` | `super`)? `::`

Path -> PathPrefix? IDENT (`::` IDENT)*
```
### Example
```
mod utils {
    fn sum(a: i32, b: i32) -> i32 {
        a + b
    }
}

fn foo() -> i32 { utils::sum(1, 2) }

let a: i32;
    
fn main() {
    let a: i32 = 1;     // Local variable
    self::a = foo();    // Access the 'a' variable from the module

}
```
In the example above we have 4 paths
- Inside sum. Both `a` and `b` are paths.
- `utils::sum` is a path composed of two segments: `utils` and `sum`
- `self::a` is a path that refers to "the item a inside the current module"
