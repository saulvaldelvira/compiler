# Abstract Syntax Tree

The AST is a syntactical representation of a [program].
It is the output of the parser.

This AST doesn't have any semantic information.
For example, it doesn't know the different types of binary operations
(arithmetic, logical, etc.). It just sees `<OPERAND> <OPERATOR> <OPERAND>`.

We try to presserve all the syntactic information of the program
(semicolons, parenthesis, etc.), and we don't make up things that aren't there.

For example, see [`ItemKind::Function`]. Its return type is an `Option`,
beacuse this program is syntactically correct:
```text
fn foo() { }
```
but this one is also correct:
```text
fn foo() -> int { ... }
```

Another example:
```
a + ( (b) * 4 )
```
There are a LOT of unnecesary parenthesis here. It's nice to have the AST preserve
this information so we can, for example, check unnecesary parenthesis.

Later, we'll lower this AST to the HIR, which will strip unnecesary syntactical
information. It'll also remove parenthesis, since their only job is to explicit
the precedence of operations, and they become useless after building the AST.

[program]: program
[`ItemKind::Function`]: https://doc.saulv.es/compiler/ast/item/enum.ItemKind.html#variant.Function
