# JadeFlow

A powerful, efficient, and simplified programming language. That is what I aspire to create in this github repository. I wish to create my own programming language to improve my competences as a developer and discover new programming concepts. I am looking to design a langage for low-level programming tasks, at least for now. JadeFlow would allow anyone to tackle complex challenges with ease. With a clear state of mind, I want to create nothing more than a language simple enough to be learned even by the worst programmer, yet useful enough to realise intellectually stimulating tasks with ease.

## Features

Variables
JadeFlow allows you to declare variables without a keyword, similar to Python.

### Data Types

JadeFlow supports the following data types:

- Boolean
- String
- Numbers (integer and float as one type)
- Objects (Classes, Functions, Arrays)
- Null
  
### Declaring Functions

You can declare functions using the fn keyword and the following syntax:

```rust
fn functionName(params) {
    // Function body
}
```

**_Note:_** Parameter type annotations will be introduced in a future update.

### Return Statement in Functions

JadeFlow allows you to return values from functions using the return statement:

```rust
return x;
```

**_Note:_** Alternatively, you can use `>x;` or simply `x` to indicate the return value as JadeFlow possesses implicit value return.

### Conditional Statements

In a future update, JadeFlow will support conditional statements like if and switch. Here's a preview of the syntax:

```rust
if x == x {
    // Code block for true condition
} else {
    // Code block for false condition
}

switch {
    case x:
        // Code block for x case
    case y:
        // Code block for y case
}
```

### Operators

JadeFlow supports various operators for arithmetic, comparison, logical operations, and assignment. Here are some examples:

- Arithmetic operators: +, -, *, /, %
- Comparison operators: ==, !=, >, <, >=, <=
- Logical operators: and, or, not
- Assignment operators: =, +=, -=, *=, /=, %=
- Increment/Decrement operators: ++, --

### Built-in Functions

In a future update, JadeFlow will provide a set of built-in functions for common tasks. Some examples include:

- Input/Output functions: print, read, prompt...
- Math functions: sqrt, abs, pow, round...
- String functions: length, substring, concat, indexOf, split...
- Arrays functions: length, splice, concat, indexOf...

### Comments

JadeFlow supports both single-line and multi-line comments:

```
# This is a single-line comment

###
This is a
multi-line comment
###

# This is a single-line comment

###
This is a
multi-line comment
###
```

### Standard Libraries

In a future update, JadeFlow will offer standard library functions and modules for tasks such as file system operations, networking, and data structures.

### Getting Started

To start using JadeFlow, follow these steps:
Wait, wait and wait for now xD
You can help in the project to one day allow everyone to use jadeflow !

### Examples

Here are some examples to get you started with JadeFlow:

```python
# Example 1: Hello World
fn main() {
    print("Hello, world!");
}

# Example 2: Fibonacci Series
fn fibonacci(n) {
    if n <= 1 {
        return n;
    }
    return fibonacci(n - 1) + fibonacci(n - 2);
}

fn main() {
    let num = 10;
    for i in 0..num {
        print(fibonacci(i));
    }
}
```

### Contributions

Contributions to JadeFlow are welcome! If you have any ideas, improvements, or bug fixes, feel free to submit a pull request. Please follow the guidelines in the CONTRIBUTING.md file.

### License

JadeFlow is released under the [MIT License](./LICENSE).

We hope you enjoy using JadeFlow! If you have any questions or need assistance, please reach out to the JadeFlow community or the project maintainers.

Happy coding!
