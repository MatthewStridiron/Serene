## Introduction

Serene is a high-level and powerful programming language that emphasizes elegance and ease of use. It’s designed to be easy to learn with its high-level data structures, intuitive syntax, strong static type-safety and useful built-in functions.

A flexible language ideal for many use cases, Serene can act as a type-safe infrastructure language like Go, an entry-level generic scripting language like Python, or a lightweight alternative to enterprise languages like Java. It places a strong emphasis on readability, opting against common abbreviations used in other languages. Serene allows developers to easily write and maintain programs without the headaches caused by lower-level languages, making it attractive for rapid application development.

## File Structure

### Folder Structure
- tests/ folder contains all the testing files
- scripts/ folder contains test script and run example
- demos/ folder contains demos of potentially useful functions

### Important Files
- serene.ml - Top level of Serene compiler
- scanner.mll - Uses ocamllex to create lexer for tokenization
- parser.mly - Converts CFG into parser table
- ast.ml - Structures abstract syntax tree
- tests.sh - Bash script which lets you run all test files consecutively
- semant.ml - Semantic checker which ensures type and scope safety
- irgen.ml - Interfaces between SAST and IR
- helper.c - Helper c functions which are used in linking

### Make Commands

```
make test
make demo1
make demo2
```

## Key Features

- Easy to read syntax
- Strong, static type safety
- Static scoping
- Strong built-in function support
  - repeatstring
  - replaceletters
  - etc
- Useful mathematical operations
  - greatestcommondenominator
  - factorial
  - etc
- Useful array support
  - maximum
  - minimum
  - product
  - etc

## Responsibilities

### Ben Peng
- Created linking system to support built-in C functions
- Worked on conditional functionality in IR Gen and SAST
- Added if statement functionality
- Worked on test cases and structuring project
- Wrote up Final Report

### Savion Sample
- Added most of the tokens for the scanner and parser for the HelloWorld program
- Worked on general things for AST and Semantics, making sure everything was working and fixing bugs
- Added the majority of the builtin functions
- Worked on IR code for arrays

### Matthew Stridiron
- Constructed the foundation for arrays from scanner phase to semantic phase.
- Built IR Gen to run basic scripts involving external function declarations, while loops, and integers.
- Gathered keywords and constructed the scanner.
- Created and debugged test scripts.

### Michael Ozymy
- Worked on loops in the scanning, parsing, and semantics phases.
- Added for loop functionality in IR Gen.
- Created tests and debugged parts of IR Gen.
- Aided in creation of LRM and final presentation.

### Carlos Raymundo
- Worked on statements and functions in the Language Reference Manual
- Worked on adding keywords and tokens to the scanner and parser modification for functions
- Created some built-in functions for strings
- Assisted in the final report and presentation of the project
