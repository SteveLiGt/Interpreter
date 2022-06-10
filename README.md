# Interpreter-from-305-class-
My semester long personal project
During this project, I will build an interpreter for a small, OCaml-like, stackbased bytecode language. 
This intepreter will be implementing in OCaml.

For this project itself, it helps me to be able to characterize the trade-offs in security /execution speed / programmer flexibility in compiler-implemented, runtime-implemented and programmer-implemented features, such as type checking, bounds checking, and memory management.

Within the project, 
it will be able to handle function call with commands 
1. Add, Sub, mul, div, rem, neg for interger operations
2. push, pop, swap for stackvalue operation
3. tostring, println for print into file
4. cat,  
        ADD |SUB | MUL | DIV | REM | NEG 
        | SWAP | POP | PUSH of stackValue | TOSTRING | PRINTLN | QUIT 
        | CAT | AND | OR | NOT | EQUAL | LESSTHAN | BIND | IF | LET | END | FUN of stackValue * stackValue | FUNEND | RETURN | CALL
