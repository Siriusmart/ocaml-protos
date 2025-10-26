# String Calculator

> Date of completion: 26/10/2025

A simple string calculator REPL.

## Features

- Binary operators `+`, `-`, `*`, `/`, `^` with precedence.
- Unary operator `-`.
- Parentheses.

## Setup

1. Git clone the repository.
    ```sh
    git clone https://github.com/siriusmart/caml-protos
    ```
2. Change directory to the project.
    ```sh
    cd caml-protos/string-calc
    ```
3. Compile the project.
    ```sh
    ocamlc tokens.ml intermediate.ml exprTree.ml calculator.mli calculator.ml main.ml -o calculator
    ```
4. Start the REPL, and start entering expressions.
    ```sh
    ./calculator
    ```
5. To exit, press Ctrl + C.
