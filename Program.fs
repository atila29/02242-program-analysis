// Learn more about F# at http://fsharp.org
open System
open ParsingUtil
open AbstractSyntaxTree




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

type ProgramGraph = Node * Node * Edge List
and Edge = Node * String * Node
and Node = int

// q-1 (end) q0 (start) -> q1 -> q2 -> q3

let convertToProgramGraph (p: Program) =
    let rec convertDeclaration ((qs, d): (int * Declaration)) =
        match d with
        | DeclarationX(x) -> ([Edge(qs, "int " + x + ";", qs+1)], qs+1) 
        | DeclarationA(name, index) -> ([Edge(qs, "int[" + string index + "] " + name + ";", qs+1)], qs+1)
        | DeclarationR(name) -> ([Edge(qs, "{int fst; int snd} " + name + ";", qs+1)], qs+1)
        | DeclarationD(d1, d2) -> 
            match convertDeclaration (qs, d1) with 
            | (lst, cnt) -> match convertDeclaration (cnt, d2) with
                            | (lst2, cnt2) -> (lst @ lst2, cnt2)

    let fixIfElse (list: Edge List) (oldend: int) (newend: int) = 
        list |> List.map (fun e -> 
        match e with
            | (qs, l, qe) when qe = oldend -> Edge(qs, l, newend)
            | (qs, l, qe) -> Edge(qs, l, qe))

    let rec convertStatements ((qs, s): (int * Statement)) =
        match s with
        | AssignmentL(l, a) -> ([Edge(qs, convertL l + ":=" + convertA a, qs + 1)], qs + 1) 
        | AssignmentR(n, a1, a2) -> ([Edge(qs, n + ":=(" + convertA a1 + "," + convertA a2, qs + 1)], qs + 1)
        | IfStatement(b, s) -> match convertStatements (qs+1, s) with
                                | (edges, n) -> (edges@ [Edge(qs, "!" + convertB b, n); Edge(qs, convertB b, qs + 1)], n+1)
        | IfElseStatement(b, s1, s2) -> match convertStatements (qs+1, s1) with
            | (edges1, n1) -> match convertStatements (n1, s2) with
                | (edges2, n2) -> (fixIfElse edges1 n1 n2 @ edges2 @ [Edge(qs, convertB b, qs+1); Edge(qs, "!" + convertB b, n1)], n2)
        | WhileStatement (b, s) -> match convertStatements (qs+1, s) with
                                    | (edges, n) -> (edges @ [Edge(qs, "!" + convertB b, n); Edge(qs, convertB b, qs + 1)], n+1)
        | Read(l) -> ([Edge(qs, "read " + convertL l, qs+1)], qs + 1)
        | Write (a) -> ([Edge(qs, "write" + convertA a, qs+1)], qs + 1)
        | Statements (s1, s2) -> 
            match convertStatements (qs, s1) with
              | (lst, cnt) -> match convertStatements (cnt, s2) with
                                | (lst2, cnt2) -> (lst @ lst2, cnt2)

    // Lots of label conversions...
    and convertL (l:L) =
        match l with
        | LabelX(x) -> x
        | LabelA(n, a) -> n + "[" + convertA a + "]"
        | LabelFstR(n) -> n + ".fst"
        | LabelSndR(n) -> n + ".snd"

    and convertA (a:A) =
        match a with
        | ArithmeticN(n) -> string n
        | ArithmeticX(x) -> x
        | ArithmeticA (n, i) -> n + "[" + string i + "]"
        | ArithmeticFstR(n) -> n + ".fst"
        | ArithmeticSndR(n) -> n + ".snd"
        | ROp (a1, aOp, a2) -> convertA a1 + convertArithOp aOp + convertA a2

    and convertB (b: B) =
        match b with
        | BoolValue(b) -> string b
        | AOp(a1, rOp, a2) -> convertA a1 + convertRelOp rOp + convertA a2
        | BOp(b1, bOp, b2) -> convertB b1 + convertBoolOp bOp + convertB b2
        | Not(b) -> "!" + convertB b

    and convertArithOp (a: ArithmeticOperator) =
        match a with
        | Plus -> "+"
        | Minus -> "-"
        | Multiply -> "*"
        | Divide -> "/"

    and convertRelOp (r: RelationalOperator) =
        match r with
        | LessThan -> "<"
        | GreaterThan -> ">"
        | LesserOrEqualTo -> "<="
        | GreaterOrEqualTo -> ">="
        | EqualTo -> "=="
        | NotEqualTo -> "!="

    and convertBoolOp (b: BooleanOperator) =
        match b with
        | AndOp -> "&"
        | OrOp -> "|"

    match p with
        | dec, stm -> match convertDeclaration (0, dec) with
                        | (edges1, cnt) -> match convertStatements (cnt, stm) with
                                           | (edges2, _) -> (edges1 @ edges2)

// let drawProgram (p: Program) =
//     let rec drawDeclaration (d: Declaration) =
//         match d with
//         | DeclarationX(x) -> "[label = \"int \\ " + x + " ;\"];\n"
//         | DeclarationA(name, index) -> "[label = \"" + name + " \\ [" +  string index + "] ;\"];\n"  // declaration of array 
//         | DeclarationR(name) -> ""
//         | DeclarationD(d, d1) ->  drawDeclaration d + drawDeclaration d1 // using lists solves issue with lastnode and endnode

//     let rec drawStatement (s: Statement) =
//         match s with
//         | AssignmentL(l, a) -> "[label = \"" + drawL l + " \\ := \\ " + drawA a + " ;\"];\n"
//         | AssignmentR(l, l1) -> ""
//         | IfStatement(b, s) -> "" // if statements was a list you could know the endnode by the length of the list? ...except for nested lists...
//         | WhileStatement (b, s) -> ""
//         | Read(l) -> ""
//         | Write (a) -> ""
//         | Statements (s, s1) -> drawStatement s + drawStatement s1 // using lists solves issue with lastnode and endnode

//     and drawL (l:L) =
//         match l with
//         | LabelX(x) -> x
//         | LabelA(name, a) -> name + "[" + drawA a + "]"
//         | LabelFstR -> ""
//         | LabelSndR -> ""

//     and drawA (a:A) =
//         match a with
//         | ArithmeticN(n) -> string n
//         | ArithmeticX(x) -> x
//         | ArithmeticA (name, index) -> name + "[" + string index + "]"
//         | ArithmeticFstR -> ""
//         | ArithmeticSndR -> ""
//         | ROp (a, aOp, a1) -> drawA a + drawArithmeticOperator aOp + drawA a1

//     and drawB (b: B) =
//         match b with
//         | True -> "true"
//         | False -> "false"
//         | AOp(a, rOp, a1) -> drawA a + drawRelationalOperator rOp + drawA a1
//         | BOp(b, bOp, b1) -> drawB b + drawBooleanOperator bOp + drawB b1
//         | Not(b) -> "!" + drawB b


//     and drawArithmeticOperator (a: ArithmeticOperator) =
//         match a with
//         | Plus -> "+"
//         | Minus -> "-"
//         | Multiply -> "*"
//         | Divide -> "/"

//     and drawRelationalOperator (r: RelationalOperator) =
//         match r with
//         | LessThan -> "<"
//         | GreaterThan -> ">"
//         | LesserOrEqualTo -> "<="
//         | GreaterOrEqualTo -> ">="
//         | EqualTo -> "=="
//         | NotEqualTo -> "!="

//     and drawBooleanOperator (b: BooleanOperator) =
//         match b with
//         | AndOp -> "&"
//         | OrOp -> "|"

//     match p with
//         | dec, stm -> 
//             "digraph program_graph3 {rankdir=TL;\n" +
//             "node [shape = circle]; q_s;\n" +
//             "node [shape = doublecircle]; q_e;\n" +
//             "node [shape = circle];\n" + 
//             drawDeclaration dec + drawStatement stm + 
//             "}"

[<EntryPoint>]
let main argv =
    
    let ast = Program(
                    DeclarationD(
                                    DeclarationA("A", 5), // int[5] A;
                                    DeclarationX("x") // int x;
                    ), 
                    Statements(
                                
                                IfElseStatement(
                                    BoolValue(true),
                                    AssignmentL(LabelX("a"), ArithmeticN(12)),
                                    AssignmentL(LabelX("b"), ArithmeticN(5))
                                ),
                                AssignmentL(LabelX("x"), ArithmeticN(4)) // x := 4;
                                //Statements(
                                //            AssignmentL(LabelA("A", ArithmeticX("x")), ArithmeticN(2)), // A[x] := 2;  alternative -> AssignmentL(LabelA("A", ArithmeticX(LabelX("x"))), ArithmeticN(2))
                                //            IfStatement(
                                //                        AOp(ArithmeticA("A", 3), EqualTo, ArithmeticN(12)), // if (A[3] == 12) { alternative -> BOp(ArithmeticA("A", ArithmeticN(3)), EqualTo(), N(12))
                                //                        AssignmentL(LabelX("x"), ArithmeticA("A", 4)) // x := A[x]; // is this by value or reference? (should be documented).
                                //            )
                                //)
                    )
    )
    

    
    let x ="
    {
       int[5] a;
       int x;
        
       x := 4;
       a[x] := 2;
       if (a[3] == 12) {
           x := a[x];
       }
    }"

    // let graph = convertToProgramGraph ast

    let tokens = parseString x
    //let y = SqlParser.start SqlLexer.tokenize lexbuf   
    // printfn "%A" tokens

    let graph = convertToProgramGraph tokens

    printfn "%A" graph

    printfn "set breakpoint to see ast value"

    // Exit code
    0
