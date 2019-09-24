
module Model

    type Program = Declaration * Statement
    
    and Statement =
        | AssignmentL of L * A
        | AssignmentR of A * A // represent the R ref?
        | Statements of Statement * Statement
        | IfStatement of B * Statement
        | WhileStatement of B * Statement
        | Read of L
        | Write of A

    and Expression =
        | L of L
        | A of A
        | B of B               
   
    and L = 
        | X
        | A of A
        | FstR
        | SndR
    and A =
        | N of int
        | X
        | A of A
        | FstR
        | SndR
        | ROp of A * ArithmeticOperator * A
    and B =
        | True
        | False
        | AOp of A * RelationalOperator * A
        | BOp of B * BooleanOperator * B
        | Not of B

    and Operator =
        | OpA of ArithmeticOperator
        | OpR of RelationalOperator
        | OpB of BooleanOperator

    and ArithmeticOperator =
            | Plus
            | Minus
            | Multiply
            | Divide
    and RelationalOperator =
            | LessThan
            | GreaterThan
            | LesserOrEqualTo
            | GreaterOrEqualTo
            | EqualTo
            | NotEqualTo    
    and BooleanOperator =
            | AndOp
            | OrOp            


    and Declaration =
            | X
            | A of int
            | R of int * int
            | D of Declaration * Declaration
