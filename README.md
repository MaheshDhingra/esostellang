# Esostellang - A Mini Esolang

Esostellang is a simple esoteric programming language implemented in Rust. It supports basic programming constructs including variables, arithmetic operators, functions, and error handling.

## Features

*   **Variables**: Declare and use variables with `let`.
*   **Operators**: Perform arithmetic operations with `+`, `-`, `*`, `/`.
*   **Functions**: Define and call functions with `fn`.
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

```
Welcome to Mini Esolang REPL!
Supports variables, functions, operators, and error handling.
Type 'exit;' to quit.
>>> let x = 10;
>>> let y = x + 5;
>>> fn add(a, b) { return a + b; }
>>> let z = add(y, 20);
>>> z;
35
>>> exit;
Bye!
```

### Running a `.stelpp` File

You can execute a `.stelpp` file by passing its path as an argument to the executable.

First, create a file (e.g., `example.stelpp`) with your Esostellang code:

```esostellang
let a = 5;
let b = 10;
fn multiply(x, y) {
    return x * y;
}
let result = multiply(a, b);
result;
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
```

### Error Handling Demonstration

**Division by Zero:**

```esostellang
let x = 10;
let y = 0;
x / y;
```

Output:

```
Runtime Error: Msg("Division by zero")
```

**Undefined Variable:**

```esostellang
undefined_var;
```

Output:

```
Runtime Error: Msg("Undefined variable 'undefined_var'")
```

**Parse Error (Missing Semicolon):**

```esostellang
let x = 10
```

Output:

```
Parse Error: Expected ';' after expression
