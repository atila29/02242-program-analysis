module ProgramGraph

open AbstractSyntaxTree

[<StructuredFormatDisplay("{AsString}")>]
type Label = 
| LabelDeclarationX of string
| LabelDeclarationA of string * int
| LabelDeclarationR of string
| LabelAssignmentL of L * A
| LabelAssignmentR of string * A * A
| LabelRead of L
| LabelWrite of A
| LabelBool of B
    override l.ToString() = 
        let convertArithOp (a: ArithmeticOperator) =
            match a with
            | Plus -> "+"
            | Minus -> "-"
            | Multiply -> "*"
            | Divide -> "/"

        let convertRelOp (r: RelationalOperator) =
            match r with
            | LessThan -> "<"
            | GreaterThan -> ">"
            | LesserOrEqualTo -> "<="
            | GreaterOrEqualTo -> ">="
            | EqualTo -> "=="
            | NotEqualTo -> "!="

        let convertBoolOp (b: BooleanOperator) =
            match b with
            | AndOp -> "&"
            | OrOp -> "|"

        let rec convertA (a:A) =
            match a with
            | ArithmeticN(n) -> string n
            | ArithmeticX(x) -> x
            | ArithmeticA (n, i) -> n + "[" + convertA i + "]"
            | ArithmeticFstR(n) -> n + ".fst"
            | ArithmeticSndR(n) -> n + ".snd"
            | ROp (a1, aOp, a2) -> convertA a1 + convertArithOp aOp + convertA a2

        let convertL (label:L) =
            match label with
            | LabelX(x) -> x
            | LabelA(n, a) -> n + "[" + convertA a + "]"
            | LabelFstR(n) -> n + ".fst"
            | LabelSndR(n) -> n + ".snd"

        let rec convertB (b: B) =
            match b with
            | BoolValue(b) -> string b
            | AOp(a1, rOp, a2) -> convertA a1 + convertRelOp rOp + convertA a2
            | BOp(b1, bOp, b2) -> convertB b1 + convertBoolOp bOp + convertB b2
            | Not(b) -> "!" + convertB b
    
        match l with
                | LabelDeclarationX(s) -> "int " + s
                | LabelDeclarationA(name, index) ->  "int[" + string index + "] " + name
                | LabelDeclarationR(name) -> "{int fst; int snd} " + name
                | LabelAssignmentL(label, a) -> convertL label + ":=" + convertA a
                | LabelAssignmentR(name, a1, a2) -> name + ":=(" + convertA a1 + "," + convertA a2 + ")"
                | LabelRead(l) -> "read " + convertL l
                | LabelWrite(a) -> "write" + convertA a
                | LabelBool(b) -> convertB b

        member l.AsString = l.ToString()

type ProgramGraph = Node * Node * Edge List
and Edge = Node * Label * Node
and Node = int


let convertToProgramGraph (p: Program) =
    let rec convertDeclaration ((qs, d): (int * Declaration)) =
        match d with
        | DeclarationX(x) -> ([Edge(qs, LabelDeclarationX(x), qs+1)], qs+1) 
        | DeclarationA(name, index) -> ([Edge(qs, LabelDeclarationA(name, index), qs+1)], qs+1)
        | DeclarationR(name) -> ([Edge(qs, LabelDeclarationR(name), qs+1)], qs+1)
        | DeclarationD(d1, d2) -> 
            match convertDeclaration (qs, d1) with 
            | (lst, cnt) -> match convertDeclaration (cnt, d2) with
                            | (lst2, cnt2) -> (lst @ lst2, cnt2)
        | _ -> ([], qs)

    let fixIfElse (list: Edge List) (oldend: int) (newend: int) = 
        list |> List.map (fun e -> 
        match e with
            | (qs, l, qe) when qe = oldend -> Edge(qs, l, newend)
            | (qs, l, qe) -> Edge(qs, l, qe))

    let rec convertStatements ((qs, s): (int * Statement)) =
        match s with
        | AssignmentL(l, a) -> ([Edge(qs, LabelAssignmentL(l, a), qs + 1)], qs + 1) 
        | AssignmentR(n, a1, a2) -> ([Edge(qs, LabelAssignmentR(n, a1, a2), qs + 1)], qs + 1)
        | IfStatement(b, s) -> match convertStatements (qs+1, s) with
                                | (edges, n) -> (edges@ [Edge(qs, LabelBool(Not(b)), n); Edge(qs, LabelBool(b), qs + 1)], n+1)
        | IfElseStatement(b, s1, s2) -> match convertStatements (qs+1, s1) with
                                        | (edges1, n1) -> match convertStatements (n1, s2) with
                                                            | (edges2, n2) -> (fixIfElse edges1 n1 n2 @ edges2 @ [Edge(qs, LabelBool(b), qs+1); Edge(qs, LabelBool(Not(b)), n1)], n2)
        | WhileStatement (b, s) -> match convertStatements (qs+1, s) with
                                    | (edges, n) -> (edges @ [Edge(qs, LabelBool(Not(b)), n); Edge(qs, LabelBool(b), qs + 1)], n+1)
        | Read(l) -> ([Edge(qs, LabelRead(l), qs+1)], qs + 1)
        | Write(a) -> ([Edge(qs, LabelWrite(a), qs+1)], qs + 1)
        | Statements (s1, s2) -> 
            match convertStatements (qs, s1) with
              | (lst, cnt) -> match convertStatements (cnt, s2) with
                                | (lst2, cnt2) -> (lst @ lst2, cnt2)


    match p with
        | dec, stm -> match convertDeclaration (0, dec) with
                        | (edges1, cnt) -> match convertStatements (cnt, stm) with
                                           | (edges2, qe) -> ProgramGraph(0, qe - 1, edges1 @ edges2)