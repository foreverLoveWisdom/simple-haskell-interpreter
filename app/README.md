# Simple Haskell Interpreter

A minimal viable interpreter written in Haskell,
designed to demonstrate core concepts of functional programming
while providing a foundation for further development.

## Goals

This project aims to:

- **Parse Input into an Abstract Syntax Tree (AST)**:
  Convert expressions like `"2 + 3 * 4"` into structured representations.
- **Evaluate the AST**: Compute results from the AST, showcasing recursion and immutability.
- **Handle Basic Errors**: Provide meaningful error messages for invalid inputs.

## Key Features

- **Basic Arithmetic Parsing**: Supports addition, multiplication, and parentheses.
- **Error Handling**: Utilizes `Maybe` and `Either` types for robust error management.
- **Extensibility**: Future enhancements can include variables and functions.

## Philosophy

In line with my approach to programming:

- Focus on eliminating distractions—90% of complexity should be ignored initially.
- Build incrementally: Start with a working MVP
  and enhance functionality based on real-world needs.
