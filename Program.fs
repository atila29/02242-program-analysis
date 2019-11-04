// Learn more about F# at http://fsharp.org
open System
open ParsingUtil
open AbstractSyntaxTree

type ProgramGraph = Node * Node * Edge List
and Edge = Node * String * Node
and Node = int


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
