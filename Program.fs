// Learn more about F# at http://fsharp.org



open System
type Program = Declaration * Statement
    
and Statement =
    | AssignmentL of L * A
    | AssignmentR of L * L // represent the R ref?
    | Statements of Statement * Statement
    | IfStatement of B * Statement
    | WhileStatement of B * Statement
    | Read of L
    | Write of A

and Expression =
    | ExpL of L
    | ExpA of A
    | ExpB of B               

and L = // by reference?
    | LabelX of String
    | LabelA of String * A
    | LabelFstR
    | LabelSndR
and A = // or by value?
    | ArithmeticN of int
    | ArithmeticX of string
    | ArithmeticA of string * int
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
        | DeclarationX of string                    // Declaration of variable 
        | DeclarationA of string * int              // declaration of array 
        | DeclarationR of int * int                 // not correct
        | DeclarationD of Declaration * Declaration 

// int[5] A;
// int x;
// x := 4;
// A[x] := 2;
// if (A[3] == 12) {
// x := A[x];
// }

let ast = Program(
                    DeclarationD(
                                    DeclarationA("A", 5), // int[5] A;
                                    DeclarationX("x") // int x;
                    ), 
                    Statements(
                                AssignmentL(LabelX("x"), ArithmeticN(4)), // x := 4;
                                Statements(
                                            AssignmentL(LabelA("A", ArithmeticX("x")), ArithmeticN(2)), // A[x] := 2;  alternative -> AssignmentL(LabelA("A", ArithmeticX(LabelX("x"))), ArithmeticN(2))
                                            IfStatement(
                                                        AOp(ArithmeticA("A", 3), EqualTo, ArithmeticN(12)), // if (A[3] == 12) { alternative -> BOp(ArithmeticA("A", ArithmeticN(3)), EqualTo(), N(12))
                                                        AssignmentL(LabelX("x"), ArithmeticA("A", 4)) // x := A[x]; // is this by value or reference? (should be documented).
                                            )
        
                                )
                    )
    )

[<EntryPoint>]
let main argv =
    

    

    printfn "set breakpoint"

    0 // return an integer exit code
