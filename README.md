# Slip: A Lisp-like Functional Language Interpreter

## Overview

Slip is an interpreter for a Lisp-like functional programming language implemented in Haskell. It provides a complete toolchain for processing S-expressions, including lexical analysis, parsing, pretty printing, and evaluation. This interpreter supports common language constructs such as numerical and Boolean constants, variable references, conditionals, function objects (closures), function application, let-bindings, and recursive definitions using `fix`.

## Features

- **Lexical Analysis:**  
  Supports tokenization of S-expressions with special handling for symbols, numbers, and comments.

- **Syntax Parsing:**  
  Utilizes Parsec for parsing S-expressions, including support for quoted expressions and lists.

- **Pretty Printer:**  
  Implements a custom pretty printer to format S-expressions for output.

- **Language Evaluation:**  
  Evaluates expressions in an environment with built-in operators for arithmetic and comparisons. Supports:
  - Numerical and Boolean literals
  - Variable lookup
  - Conditionals (if expressions)
  - Function creation (`fob`) and application
  - Let-bindings and mutually recursive definitions (`fix`)

- **REPL and File Execution:**  
  Provides a `run` function to execute multiple S-expressions from a file and print their evaluated results.

## Installation

Ensure you have the Glasgow Haskell Compiler (GHC) installed. Clone the repository and compile the source code:

```bash
git clone https://github.com/yourusername/slip.hs.git
cd slip.hs
ghc --make slip.hs -o slip
