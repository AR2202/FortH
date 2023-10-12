# FortH

Learning to write a Forth interpreter

## USAGE

The purpose of this project is to learn about interpreters and compilers by writing one. It is not intended to be used as a real-world interpreter.

IF for whatever reason you would like to try it anyway, it is recommended to have the Haskell tool stack installed. In this case, simply run:

`stack run`

This should bring up the REPL displaying this message 

Welcome to FortHi

type ':q' to exit`

Followed by the input prompt:

FortHi>

Type a line of Forth code and press Enter to interpret it.

## Case sensitive 

This implementation of Forth is case-sensitive and keywords are in ALL CAPITALS

## Simple Arithmetic

Forth uses postfix notation.

### Examples

`1 2 +`

output: 3

`2 3 *`

output: 6

`1 2 -`

output: -1

## Boolean Operators

0 is interpreted as False, all other numbers are interpreted as True. This interpreter supports AND, OR, XOR, NOT (all capitalized)

### Examples

`1 0 AND`

output: 0

`1 0 OR`

output: 1

`1 0 XOR`

output: 1

`1 1 XOR`

output: 0

`1 NOT`

output: 0

## Stack Operations

Forth is a stack based language. If a number is entered, it is stored on the stack. Binary operators such as + take two numbers off the top of the stack and push the result of the operation to the top of the stack.

The follwing stack operations are supported: DROP, DUP, SWAP, OVER, ROT, INVERT

### DROP
Removes the top element from the stack.

### DUP
Duplicates the top element of the stack.

### SWAP
Swaps the top two elements of the stack.

### OVER
Duplicates the top element of the stack and moves one copy below the second element.

### ROT
Rotates the first 3 elements.

### INVERT
Inverts the first element of the stack. Equivalent to NOT.

### Printing the stack
`.` prints the top element of the stack and drops it.
`.s` prints the entire stack without dropping it.

### EXAMPLES

`1 2 3`

`.s`

output:

1

2

3

`1 2 3`

`DUP`

`.s`

output: 

1

2

3

3
## Conditionals

### IF .. THEN
IF statements take the top off the stack and execute the code enclosed in IF and THEN iff the top stack element evaluates to TRUE.

### IF .. ELSE .. THEN
Same as IF ..THEN, but if the top stack element is FALSE, the code between ELSE and THEN is executed instead.

## Loops

### DO LOOP
DO .. LOOP takes 2 elements off the stack. The top is the start of the index. The code between DO and LOOP is executed until the index reaches the value of the second stack element. Both elements get dropped. Within the loop, the index can be accessed by I.

### DO +LOOP
DO .. +LOOP is similar to DO .. LOOP, but the index is incremented by the value that is on top of the stack at the end of the Loop instead of always being incremented by 1 as in DO .. LOOP. The increment can be negative.

### BEGIN .. UNTIL
BEGIN .. UNTIL executes the code until the top of the stack evaluates to True at the end of one iteration.

### Examples

`10 1 DO I . LOOP`

output: 123456789

`10 BEGIN DUP . CR 1 - DUP 0 = UNTIL`

output: 

10

9

8

7

6

5

4

3

2

1


`50 0 DO  I .  5 +LOOP`

output: 051015202530354045

## Declaring and assigning variables

Variables are declared with the VAR keyword followed by the variable_name. They are assigned a value with ! and the value is retrieved with @

### Examples

`VAR myvar`

`5 myvar !`

`myvar @`

output: 5



`1 myvar !`

`myvar @`

output: 1

## Arrays

Arrays are contiguous chunks of memory. They are created in the same way as variables. Additional memory can be allotted by CELLS ALLOT. They are initialized to 0. Values can be assigned to each cell individually with ! or to several cells at once using the , operator. The first element of the array can be retrieved like a variable with @. The subsequent cells' values can be retieved by adding CELL or CELLS to the start position
### Examples

`VARIABLE myarray`

`myarray 5 CELLS ALLOT`

`myarray @`

output: 0

`myarray 1 , 2 , 3 , 4 , 5 ,`

`myarray @`

output: 1

`myarray CELL + @`

output: 2

`myarray 2 CELLS + @`

output: 3
## Strings

String literals can be printed with ."mystring"

`."hello world"`

output: hello world

When storing strings, they have to consist of ASCII characters. The characters of the string are converted to their ASCII codes and the string is stored as an array of ASCII characters.  When retrieving the string, e.g. for printing, the start address and the length have to be provided to the TYPE word. S"mystring" saves mystring in memory and returns the address and length of the string on the stack. Immediately following by TYPE types the string to the terminal. If the string needs to be accessed later, both the address and the length need to be stored. 

### Examples

`S"hello world"`

`.s`

output:

1

11

`TYPE`

output: hello world

`1 11 TYPE`

output: hello world

`1 5 TYPE`

output: hello

`7 5 TYPE`

output: world

## Defining functions

A function (word) is defined by:

`: function_name function_body ;`

The spaces after the colon and before the semicolon are mandatory. Capitalization of function names is optional.

Function can be called by `function_name`

### Examples

`: add1 1 + ;`

`3 add1`

output: 4

Built in operators and words can be re-defined.

`: + * ;`

`1 2 +`

output: 2

`: fizzbuzz 1 DO I 15 MOD 0 = IF ."fizzbuzz" ELSE I 3 MOD 0 = IF ."fizz" ELSE I 5 MOD 0 = IF ."buzz" ELSE I . THEN THEN THEN CR LOOP ;`

`16 fizzbuzz`

output:

1

2

fizz

4

buzz

fizz

7

8

fizz

buzz

11

fizz

13

14

fizzbuzz

### Execution Token

Prefixing the function_name by \` returns the function's execution token. The function can then be called by providing the token followed by EXECUTE.

### Examples

\`add1

output: 25

`2 25 EXECUTE`

output: 3

### Recursion

Functions cannot refer to themselves in the definition body by name. Recursive functions can be defined with the RECURSE keyword in the function definition.

### Examples

`: rec DUP . ." " DUP 10 < IF 1 + RECURSE THEN ;`

`0 rec`

output: 0 1 2 3 4 5 6 7 8 9 10