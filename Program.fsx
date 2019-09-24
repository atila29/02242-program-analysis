// Learn more about F# at http://fsharp.org



open System
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
    | ExpL of L
    | ExpA of A
    | ExpB of B               

and L = 
    | LabelX of String
    | LabelA of String * A
    | LabelFstR
    | LabelSndR
and A =
    | ArithmeticN of int
    | ArithmeticX
    | ArithmeticA of A
    | ArithmeticFstR
    | ArithmeticSndR
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
        | DeclarationX of string
        | DeclarationA of string * int
        | DeclarationR of int * int
        | DeclarationD of Declaration * Declaration



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"

    let ast = Program(
        DeclarationD(DeclarationA("A", 5), DeclarationX("x")), 
        Statements(
            AssignmentL(X("x"), A(4)), 
            Statements(
                AssignmentL("x", 4),
                IfStatement(
                    RelationalOperator(A("A", 3), EqualTo, N(12)), 
                    AssignmentL("x", 4)
                )
            )
        )
    )





    0 // return an integer exit code
