# Intepreter

Two phases:
- Front end, AST
    - Lexical analysis, lexer
    - Syntactic analysis, parser
    - Semantic analysis
- Back end, translate AST to machine code

The `lexer` transform character streat to token stream. Then `parser` transform
 token stream to AST. Then we will do semantic analysis on AST, such as 
 accept or reject the program, etc.

After the front end, Intepreter begins executing code using AST, while a complier
 begins translate code into machine code.

## Example
