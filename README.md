# Esostellang - A Mini Esolang

Esostellang is a simple esoteric programming language implemented in Rust. It features a unique syntax and supports basic programming constructs including variables, arithmetic operators, functions, and error handling.

## Features

*   **Unique Syntax**:
    *   Variable declarations: `var` (instead of `let`)
    *   Function definitions: `func` (instead of `fn`)
    *   Return statements: `ret` (instead of `return`)
    *   Print function: `shit("message");` or `shit(variable);`
*   **Variables**: Declare and use variables with `var`.
*   **Operators**: Perform arithmetic operations with `+`, `-`, `*`, `/`.
*   **Functions**: Define and call functions with `func`.
*   **Error Handling**: Includes runtime error messages for issues like division by zero or undefined variables/functions.

## How to Build and Run

### Prerequisites

*   **Rust**: Ensure you have Rust and Cargo installed. You can install them from [rustup.rs](https://rustup.rs/).
*   **C++ Build Tools (Windows only)**: If you are on Windows, you might need to install "Build Tools for Visual Studio" (2017 or later) with the "Desktop development with C++" workload.

### Building the Project

Navigate to the root directory of the project in your terminal and run:

```bash
cargo build
```

This will compile the `esostellang` executable.

### Running the REPL (Interactive Mode)

To start the interactive REPL (Read-Eval-Print Loop), run:

```bash
cargo run
```

You can then type commands directly into the prompt. Type `exit;` to quit.

Example REPL session:

```esostellang
Welcome to Mini Esolang REPL!
Supports variables, functions, operators, and error handling.
Type 'exit;' to quit.
>>> var x = 10;
>>> var y = x + 5;
>>> func add(a, b) { ret a + b; }
>>> var z = add(y, 20);
>>> shit(z);
35
>>> shit("Hello from REPL!");
Hello from REPL!
>>> exit;
Bye!
```

### Running a `.stelpp` File

You can execute a `.stelpp` file by passing its path as an argument to the executable.

First, create a file (e.g., `example.stelpp`) with your Esostellang code:

```esostellang
var a = 5;
var b = 10;
func multiply(x, y) {
    ret x * y;
}
var result = multiply(a, b);
shit(result);
shit("Hello, Esostellang!");
```

Then, run the file using:

```bash
cargo run -- example.stelpp
```

Or, if you have already built the project, you can directly run the executable:

```bash
target/debug/esostellang example.stelpp
```

The output for the `example.stelpp` file above would be:

```
50
Hello, Esostellang!
```

### Error Handling Demonstration

**Division by Zero:**

```esostellang
var x = 10;
var y = 0;
shit(x / y);
```

Output:

```
Runtime Error: Msg("Division by zero")
```

**Undefined Variable:**

```esostellang
shit(undefined_var);
```

Output:

```
Runtime Error: Msg("Undefined variable 'undefined_var'")
```

**Parse Error (Missing Semicolon):**

```esostellang
var x = 10
```

Output:

```
Parse Error: Expected ';' after expression
