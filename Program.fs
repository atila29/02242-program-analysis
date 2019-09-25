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
        | DeclarationR of string                    // Declaration of record
        | DeclarationD of Declaration * Declaration 

// int[5] A;
// int x;
// x := 4;
// A[x] := 2;
// if (A[3] == 12) {
// x := A[x];
// }

// digraph program_graph3 {rankdir=TL;
// node [shape = circle]; q_s;
// node [shape = doublecircle]; q_e;
// node [shape = circle];
//     q_s -> q_1 [label = "int[5] \ A;"];
//     q_1 -> q_2 [label = "int \ x;"];
//     q_2 -> q_3 [label = "x := 4;"];
//     q_3 -> q_4 [label = "A[x] := 2;"];
//     q_4 -> q_5 [label = "A[3] == 12"];
//     q_5 -> q_e [label = "x := A[x];"];
//     q_4 -> q_e [label = "!(A[3] == 12)"];
// }

let drawProgram (p: Program) =
    let rec drawDeclaration (d: Declaration) =
        match d with
        | DeclarationX(x) -> "[label = \"int \\ " + x + " ;\"];\n"
        | DeclarationA(name, index) -> "[label = \"" + name + " \\ [" +  string index + "] ;\"];\n"  // declaration of array 
        | DeclarationR(name) -> ""
        | DeclarationD(d, d1) ->  drawDeclaration d + drawDeclaration d1 // using lists solves issue with lastnode and endnode

    let rec drawStatement (s: Statement) =
        match s with
        | AssignmentL(l, a) -> "[label = \"" + drawL l + " \\ := \\ " + drawA a + " ;\"];\n"
        | AssignmentR(l, l1) -> ""
        | IfStatement(b, s) -> ""
        | WhileStatement (b, s) -> ""
        | Read(l) -> ""
        | Write (a) -> ""
        | Statements (s, s1) -> drawStatement s + drawStatement s1 // using lists solves issue with lastnode and endnode

    and drawL (l:L) =
        match l with
        | LabelX(x) -> x
        | LabelA(name, index) -> ""
        | LabelFstR -> ""
        | LabelSndR -> ""

    and drawA (a:A) =
        match a with
        | ArithmeticN(n) -> string n
        | ArithmeticX(x) -> ""
        | ArithmeticA (name, index) -> ""
        | ArithmeticFstR -> ""
        | ArithmeticSndR -> ""
        | ROp (a, aOp, a1) -> drawA a + drawArithmeticOperator aOp + drawA a1

    and drawB (b: B) =
        match b with
        | True -> ""
        | False -> ""
        | AOp(a, rOp, a1) -> drawA a + drawRelationalOperator rOp + drawA a1
        | BOp(b, bOp, b1) -> drawB b + drawBooleanOperator bOp + drawB b1
        | Not(b) -> "!" + drawB b


    and drawArithmeticOperator (a: ArithmeticOperator) =
        match a with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"

    and drawRelationalOperator (r: RelationalOperator) =
        match r with
        | LessThan -> "<"
        | GreaterThan -> ">"
        | LesserOrEqualTo -> "<="
        | GreaterOrEqualTo -> ">="
        | EqualTo -> "=="
        | NotEqualTo -> "!="

    and drawBooleanOperator (b: BooleanOperator) =
        match b with
        | AndOp -> "&"
        | OrOp -> "|"

    match p with
        | dec, stm -> 
        "digraph program_graph3 {rankdir=TL;
node [shape = circle]; q_s;
node [shape = doublecircle]; q_e;
node [shape = circle];\n" + drawDeclaration dec + drawStatement stm + "}"

[<EntryPoint>]
let main argv =
    
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
    
    let drawing = drawProgram ast

    printfn "%s" drawing

    0 // return an integer exit code
