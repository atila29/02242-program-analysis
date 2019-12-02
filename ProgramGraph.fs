module ProgramGraph

open AbstractSyntaxTree
open System.Text

[<StructuredFormatDisplay("{AsString}")>]
type Action = 
| ActionDeclarationX of string
| ActionDeclarationA of string * int
| ActionDeclarationR of string
| ActionAssignmentL of L * A
| ActionAssignmentR of string * A * A
| ActionRead of L
| ActionWrite of A
| ActionBool of B
  override l.ToString() = 
      match l with
      | ActionDeclarationX(x) -> "int " + x
      | ActionDeclarationA(x, n) -> "int[" + string n + "] " + x
      | ActionDeclarationR(x) -> "{int fst; int snd} " + x
      | ActionAssignmentL(l, a) -> string l + " := " + string a
      | ActionAssignmentR(x, a1, a2) -> x + " := (" + string a1 + ", " + string a2 + ")"
      | ActionRead(l) -> "read " + string l
      | ActionWrite(a) -> "write " + string a
      | ActionBool(b) -> string b

      member l.AsString = l.ToString()



type ProgramGraph = Node * Node * Edge List
and Edge = Node * Action * Node
and Node = int


let variables (pg: ProgramGraph): string Set =
  let variablesSingle (action: Action): Set<string> = 
    let rec variablesA (a: A) = 
      match a with
      | ArithmeticX(x) -> Set.singleton x
      | ArithmeticFstR(x) -> Set.singleton (x + ".fst")
      | ArithmeticSndR(x) -> Set.singleton (x + ".snd")
      | ArithmeticA(x, a2) -> (Set.singleton x) + (variablesA a2)
      | ROp(a1, _, a2) -> (variablesA a1) + (variablesA a2)
      | _ -> Set.empty

    let variablesL (l: L) = 
      match l with
      | LabelX(x) -> Set.singleton x
      | LabelFstR(x) -> Set.singleton (x + ".fst")
      | LabelSndR(x) -> Set.singleton (x + ".snd")
      | LabelA(x, a) -> (Set.singleton x) + (variablesA a)

    let variablesB (b: B) =
      match b with
      | AOp(a1, _, a2) -> variablesA a1 + variablesA a2
      | _ -> Set.empty

    match action with
    | ActionDeclarationX(x)
    | ActionDeclarationA(x, _) -> Set.singleton x
    | ActionDeclarationR(x) -> Set.ofList [x + ".fst"; x + ".snd"]
    | ActionAssignmentL(l, a) -> (variablesL l) + (variablesA a)
    | ActionAssignmentR(x, a1, a2) -> Set.ofList [x + ".fst"; x + ".snd"] + variablesA a1 + variablesA a2
    | ActionRead(l) -> variablesL l
    | ActionWrite(a) -> variablesA a
    | ActionBool(b) -> variablesB b

  let rec variablesRec (edges: Edge List) (set: Set<string>) =
    match edges with
    | (_, action, _) :: tail -> variablesRec tail (set + variablesSingle action)
    | [] -> set
  
  let (_, _, edges) = pg;
  variablesRec edges Set.empty

let variableInAction (action: Action) : string Set option =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _) -> Some (Set.singleton x)
  | ActionDeclarationR(x)
  | ActionAssignmentR(x, _, _) -> Some (Set.ofList [x + ".fst"; x + ".snd"])
  | ActionRead(x)
  | ActionAssignmentL(x, _) -> match x with
                                | LabelX(x)
                                | LabelA(x, _) -> Some (Set.singleton x)
                                | LabelFstR(x) -> Some (Set.singleton (x + ".fst"))
                                | LabelSndR(x) -> Some (Set.singleton (x + ".snd"))
  | _ -> None

let nodes (pg: ProgramGraph): Node List = 
  let (qs, qe, _) = pg;
  [qs .. qe]


let convertToProgramGraph (p: Program) =
    let rec convertDeclaration ((qs, d): (int * Declaration)) =
        match d with
        | DeclarationX(x) -> ([Edge(qs, ActionDeclarationX(x), qs+1)], qs+1) 
        | DeclarationA(name, index) -> ([Edge(qs, ActionDeclarationA(name, index), qs+1)], qs+1)
        | DeclarationR(name) -> ([Edge(qs, ActionDeclarationR(name), qs+1)], qs+1)
        | DeclarationD(d1, d2) -> 
            match convertDeclaration (qs, d1) with 
            | (lst, cnt) -> match convertDeclaration (cnt, d2) with
                            | (lst2, cnt2) -> (lst @ lst2, cnt2)
        | _ -> ([], qs)

    let fixLastElement (list: Edge List) (oldend: int) (newend: int) = 
        list |> List.map (fun e -> 
        match e with
            | (qs, l, qe) when qe = oldend -> Edge(qs, l, newend)
            | (qs, l, qe) -> Edge(qs, l, qe))

    

    let rec convertStatements ((qs, s): (int * Statement)) =
        match s with
        | AssignmentL(l, a) -> ([Edge(qs, ActionAssignmentL(l, a), qs + 1)], qs + 1) 
        | AssignmentR(n, a1, a2) -> ([Edge(qs, ActionAssignmentR(n, a1, a2), qs + 1)], qs + 1)
        | IfStatement(b, s) -> match convertStatements (qs+1, s) with
                                | (edges, n) -> (edges@ [Edge(qs, ActionBool(Not(b)), n); Edge(qs, ActionBool(b), qs + 1)], n)
        | IfElseStatement(b, s1, s2) -> match convertStatements (qs+1, s1) with
                                        | (edges1, n1) -> match convertStatements (n1, s2) with
                                                            | (edges2, n2) -> (fixLastElement edges1 n1 n2 @ edges2 @ [Edge(qs, ActionBool(b), qs+1); Edge(qs, ActionBool(Not(b)), n1)], n2)
        | WhileStatement (b, s) -> match convertStatements (qs+1, s) with
                                    | (edges, n) -> (fixLastElement edges n qs @ [Edge(qs, ActionBool(Not(b)), n); Edge(qs, ActionBool(b), qs + 1)], n)
        | Read(l) -> ([Edge(qs, ActionRead(l), qs+1)], qs + 1)
        | Write(a) -> ([Edge(qs, ActionWrite(a), qs+1)], qs + 1)
        | Statements (s1, s2) -> 
            match convertStatements (qs, s1) with
              | (lst, cnt) -> match convertStatements (cnt, s2) with
                                | (lst2, cnt2) -> (lst @ lst2, cnt2)


    match p with
        | dec, stm -> match convertDeclaration (0, dec) with
                        | (edges1, cnt) -> match convertStatements (cnt, stm) with
                                           | (edges2, qe) -> ProgramGraph(0, qe, edges1 @ edges2)

let printVizGraph (pg: ProgramGraph) : string =
  let (startNode, endNode, edges) = pg
  let init  = StringBuilder("digraph program_graph {rankdir=TL;\nn0 [label= \"\", shape=none,height=.0,width=.0] n0 -> q_" + string startNode + ";\nnode [shape = doublecircle]; q_" + string endNode + ";\nnode [shape = circle]\n")

  let printEdge (edge: Edge) =
    match edge with
    | (qs, action, qe) -> "q_" + string qs + " -> q_" + string qe + " [label = \"" + string action + ";\"]\n"

  let result = edges
                |> List.map printEdge
                |> Seq.fold(fun (sb: StringBuilder) s -> sb.Append(s)) init

  result.Append("}").ToString()
  