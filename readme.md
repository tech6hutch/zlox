An implementation of [Lox](https://craftinginterpreters.com/) in [Zig](https://ziglang.org/).

# Lox extensions

## Print function
I replaced the `print` statement with a native `print()` function. It accepts multiple arguments.

The standard print statement can be reenabled by setting the relevant constant in [./src/common.zig](./src/common.zig).

## Switch statement
See [./tests/switch_test.txt](./tests/switch_test.txt). I replaced the traditional colon after cases with putting the value in parentheses, making the language more visually consistent (maybe). I also eschewed adding a `default` keyword and instead reused `else`, like Zig.

## Break/continue
They work like in most languages.

## Optional static typing
Local variables may be annotated with a type, like `var my_str: string = "";`. Anytime the variable is assigned (including the initial assignment), it checks (at runtime) whether it's the correct type. The types are `bool`, `float`, `string`, and `object` (including functions and classes).

Global variables are not supported. The problem is that they may not be declared yet when they're used, in which case the type isn't known. I could only type-check assignments after their declaration in the source code, or require you to declare them before use, but a more robust solution would require a compiler that does multiple passes (i.e., to parse global declarations first _and then_ parse their bodies).
