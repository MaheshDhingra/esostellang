# Esostellang - A Mini Esolang

Esostellang is a simple esoteric programming language implemented in Rust. It features a unique syntax and supports basic programming constructs including variables, arithmetic operators, functions, control flow, and error handling.

## Features

*   **Unique Syntax**:
    *   Variable declarations: `var` (instead of `let`)
    *   Function definitions: `func` (instead of `fn`)
    *   Return statements: `ret` (instead of `return`)
    *   Print function: `shit("message");` or `shit(variable);`
*   **Data Types**:
    *   Integers (`5`, `10`)
    *   Strings (`"hello"`, `"world"`)
    *   Booleans (`true`, `false`)
*   **Variables**: Declare and use variables with `var`.
*   **Variable Re-assignment**: Re-assign values to existing variables using the `=` operator.
*   **Operators**:
    *   Arithmetic: `+`, `-`, `*`, `/`
    *   Comparison: `==` (equal), `!=` (not equal), `<` (less than), `>` (greater than), `<=` (less than or equal), `>=` (greater than or equal)
*   **Functions**: Define and call functions with `func`.
*   **Control Flow**: `if (condition) { ... } else { ... }` statements for conditional execution.
*   **User Input**: `gimme()` function to read a line of text from the user.
*   **Error Handling**: Includes runtime error messages for issues like division by zero, undefined variables/functions, or invalid operations.

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
>>> if (z > 30) { shit("Z is big!"); } else { shit("Z is small."); }
Z is big!
>>> var name = gimme();
Your Name
>>> shit("Hello, " + name + "!");
Hello, Your Name!
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
shit(result); // Should print 50

shit("--- Testing If/Else and Booleans ---");

var num = 7;
if (num > 5) {
    shit("Number is greater than 5!");
} else {
    shit("Number is not greater than 5.");
}

var is_seven = num == 7; // Example of boolean assignment
if (is_seven) {
    shit("Number is seven.");
} else {
    shit("Number is not seven.");
}

// Example of variable re-assignment
var counter = 1;
shit("Counter before re-assignment: ");
shit(counter); // Should print 1
counter = counter + 1;
shit("Counter after re-assignment: ");
shit(counter); // Should print 2

shit("--- Testing User Input (gimme) ---");
shit("What's your favorite number?");
var fav_num_str = gimme(); // User will type input here
shit("You said your favorite number is: ");
shit(fav_num_str);

// Example of comparison operators
shit(5 == 5);   // true
shit(5 != 10);  // true
shit(10 > 5);   // true
shit(5 < 10);   // true
shit(5 >= 5);   // true
shit(10 <= 5);  // false
```

Then, run the file using:

```bash
cargo run -- example.stelpp
```

Or, if you have already built the project, you can directly run the executable:

```bash
target/debug/esostellang example.stelpp
```

The output for the `example.stelpp` file above (assuming user inputs `42` for `gimme()`) would be:

```
50
--- Testing If/Else and Booleans ---
Number is greater than 5!
Number is seven.
--- Testing User Input (gimme) ---
What's your favorite number?
You said your favorite number is: 
42
--- Testing Variable Re-assignment ---
Counter before re-assignment: 
1
Counter after re-assignment: 
2
true
true
true
true
true
false
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
```

**Invalid If Condition:**

```esostellang
if ("hello") { shit("This won't work"); }
```

Output:

```
Runtime Error: Msg("If condition must be a boolean")
