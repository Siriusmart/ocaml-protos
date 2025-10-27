# JSON Parser and Serialiser

> Date of completion: 27/10/2025

JSON parser and serialiser

## Features

- ALl JSON features (except infinity)
- JSON parsing from file.

## Setup

1. Git clone the repository.
    ```sh
    git clone https://github.com/siriusmart/caml-protos
    ```
2. Change directory to the project.
    ```sh
    cd caml-protos/json-parser
    ```
3. Compile the project.
    ```sh
    ocamlc tokens.ml fold.ml expr.ml parser.ml serialiser.ml main.ml -o json-parser
    ```
4. Start the REPL, and start entering a file path to a JSON file. I even included one for you at `hello.json`
    ```sh
    ./json-parser
    ```
5. To exit, press Ctrl + C.
