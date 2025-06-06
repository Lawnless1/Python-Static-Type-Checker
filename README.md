# Python-Static-Type-Checker

## Overview

The `Python-Static-Type-Checker` is a CLI tool designed to perform static type checking for Python code. It leverages Python's Abstract Syntax Tree (AST) and provides type inference, error handling, and syntax validation. The project is implemented in OCaml and integrates with Python for syntax checks.

## Features

- **Syntax Checking**: Validates Python code syntax using Python's built-in `py_compile` module.
- **Static Type Checking**: Infers types and checks for type mismatches in Python code.
- **Error Reporting**: Provides detailed error messages for syntax and type errors.
- **Debug Mode**: Offers a debug mode for verbose output during type checking.

## Installation

### Prerequisites

- Python 3.x
- OCaml (recommended version: 4.14.0)
- [Dune](https://dune.readthedocs.io/en/stable/) build system
- [Opam](https://opam.ocaml.org) package manager
- `pyre-ast` Python library

### Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/Lawnless1/Python-Static-Type-Checker.git
   cd Python-Static-Type-Checker
   ```

2. Install OCaml dependencies:
    ```ocaml
    opam switch create ./ 4.14.0 --deps-only --with-test
    dune build @install
    ```

3. Install the pyre-ast library:
   ```
   opam install pyre-ast
   ```

### Usage
Running the Static Type Checker
To run the static type checker, use the main.sh script:
```bash
./main.sh <file_path>
```
**Options:**

-d: Enable debug mode for verbose output.
<file_path>: Path to the Python file to be checked.
Example:
```bash
main.sh -d example.py
```
**Output**

The tool performs two checks:

1. **Syntax Check**: Validates the syntax of the Python file.
2. **Type Check**: Performs static type checking and reports any type mismatches.

**Debug Mode**

Enable debug mode using the `-d` flag to see detailed information about the type inference process.

### Project Structure
main.sh: Bash script to run the static type checker.

* `bin/main.ml`: OCaml implementation of the type checker, including type inference and error handling.
* `lib/`: Contains supporting OCaml modules for parsing and type checking.
* `pyre-ast/`: Includes Python AST parsing logic and tests.
test/: Contains test inputs and scripts for validating the type checker.
* `test/`: Contains test inputs and scripts for validating the type checker.

### License
This project is licensed under the Apache License 2.0. See the [LICENSE](LICENSE) file for details.

The `pyre-ast` library used in this project is licensed under the MIT License. See [pyre-ast/LICENSE](Python-static-Type-Checker/pyre-ast/LICENSE) for details.

### Acknowledgments
* The `pyre-ast` library for Python AST parsing.
* The OCaml community for tools and libraries.
