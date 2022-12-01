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

## Functions
- printf() - prints floats
- printstring() - prints strings
- printboolean() - prints booleans
- print() - prints integers
- substring("hello", 1, 3) - obtains the substring from index 1 to index 3 of the string "hello"
- printarray(ar, 4) - prints all of the elements in the array. 4 denotes the length of the array.
- maximum(ar, 4) - obtains the maximum number from array 'ar.' 4 denotes the length of the array
- sum(ar, 4) - obtains the sum of all the numbers in the array, 'ar.'  4 denotes the length of the array
- reverse(ar, 4) - reverses the elements within array 'ar.' 4 denotes the array length
- absolute(5) - absolute value of 5
- power(2,3) - computes 2 to the 3rd power
- fibonacci(7) - computes the 7th fibonacci number
- touppercase() - converts every letter in a string to upper case
- tolowercase() - converts every letter in a string to lower case
- randomnumber(6,10) - outputs a random number between 6 and 10
- randomletter() - outputs a random letter
- repeatstring("dog",3) - repeats the string "dog" three times
- greatestcommondenominator(81,18) - computes the GCD between 81 and 18
- printarray(range(1,8), 7);- range works similarly to python's range by outputting an array between 1 and 8. 7 denotes the array's length
- printarray(rangewithstep(1,8,2), 4) -  range works similarly to python's range by outputting an array between 1 and 8, except with a step size of 2. 4 denotes the array's length.
- distance(10,10,7,6) - computes distance of coordinates (10,10) and (7,6).
- printstring(concatenate("abc","def")) - prints the concatenation of "abc" and "def", which is "abcdef"
- findchar("Serene", 'n') - outputs the index of where the first 'n' is located.
- replaceletters("Serene", 'e', 'i') - replaces all instances of 'e' with 'i'
- scrambleletters("Serene") - scrambles the letters within the string "Serene"

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
- Gathered keywords and constructed the scanner/parser.
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
