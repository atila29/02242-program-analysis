module Analyses.DetectionOfSigns

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree

type Sign = Negative | Zero | Positive
and DetectionOfSignsVar = string * Sign Set
and DetectionOfSignsArr = string * Sign Set
and DetectionOfSigns =  DetectionOfSignsVar Set * DetectionOfSignsArr Set


// {x, {-,0}}
// {x, {+}}

let initial (vars: string Set) (arrs: string Set) : DetectionOfSigns Set =
  let dsVar = vars |> Set.map (fun x -> DetectionOfSignsVar(x, Set.ofList [Negative; Zero; Positive]))
  let dsArr = arrs |> Set.map (fun x -> DetectionOfSignsArr(x, Set.ofList [Negative; Zero; Positive]))
  (dsVar, dsArr)

let signOf (n: int): Sign =
  if n < 0 then
    Negative
  else if n = 0 then
    Zero
  else
    Positive

let plus (s1: Sign) (s2: Sign) : Sign Set =
  match s1 with
  | Negative when s2 = Negative -> Set.singleton Negative
  | Negative when s2 = Zero -> Set.singleton Negative
  | Negative when s2 = Positive -> Set.ofList [Negative; Zero; Positive]
  | Zero when s2 = Negative -> Set.singleton Negative
  | Zero when s2 = Zero -> Set.singleton Zero
  | Zero when s2 = Positive -> Set.singleton Positive
  | Positive when s2 = Negative -> Set.ofList [Negative; Zero; Positive]
  | Positive when s2 = Zero -> Set.singleton Positive
  | Positive when s2 = Positive -> Set.singleton Positive
  | _ -> failwith "Signs not matced correctly in + table"

let minus (s1: Sign) (s2: Sign) : Sign Set =
  match s1 with
  | Negative when s2 = Negative -> Set.ofList [Negative; Zero; Positive]
  | Negative when s2 = Zero -> Set.singleton Negative
  | Negative when s2 = Positive -> Set.singleton Negative
  | Zero when s2 = Negative -> Set.singleton Positive
  | Zero when s2 = Zero -> Set.singleton Zero
  | Zero when s2 = Positive -> Set.singleton Negative
  | Positive when s2 = Negative -> Set.singleton Positive
  | Positive when s2 = Zero -> Set.singleton Positive
  | Positive when s2 = Positive -> Set.ofList [Negative; Zero; Positive]
  | _ -> failwith "Signs not matced correctly in - table"

let times (s1: Sign) (s2: Sign) : Sign Set =
  match s1 with
  | Negative when s2 = Negative -> Set.singleton Positive
  | Negative when s2 = Zero -> Set.singleton Zero
  | Negative when s2 = Positive -> Set.singleton Negative
  | Zero when s2 = Negative -> Set.singleton Zero
  | Zero when s2 = Zero -> Set.singleton Zero
  | Zero when s2 = Positive -> Set.singleton Zero
  | Positive when s2 = Negative -> Set.singleton Negative
  | Positive when s2 = Zero -> Set.singleton Zero
  | Positive when s2 = Positive -> Set.singleton Positive
  | _ -> failwith "Signs not matced correctly in * table"

let division (s1: Sign) (s2: Sign) : Sign Set =
  match s1 with
  | Negative when s2 = Negative -> Set.singleton Positive
  | Negative when s2 = Zero -> Set.empty
  | Negative when s2 = Positive -> Set.singleton Negative
  | Zero when s2 = Negative -> Set.singleton Zero
  | Zero when s2 = Zero -> Set.empty
  | Zero when s2 = Positive -> Set.singleton Zero
  | Positive when s2 = Negative -> Set.singleton Negative
  | Positive when s2 = Zero -> Set.empty
  | Positive when s2 = Positive -> Set.singleton Positive
  | _ -> failwith "Signs not matced correctly in / table"

let powerset (s1: Sign Set) (s2: Sign Set) (op: ArithmeticOperator) : Sign Set = 
  Set.foldBack (fun e1 acc -> 
    Set.foldBack (
      fun e2 acc2 -> 
        match op with
        | Plus -> acc2 + plus e1 e2
        | Minus -> acc2 + minus e1 e2 
        | Multiply -> acc2 + times e1 e2
        | Divide -> acc2 + division e1 e2
      ) s2 acc
    ) s1 Set.empty

let rec mapArith (arith: A) (ds: DetectionOfSigns AnalysisAssignment) (qs: Node) =
  let getResult (x: string) (ds: DetectionOfSigns AnalysisAssignment) =
    ds.Item qs |> Set.toList |> Map.ofList |> Map.find x

  match arith with
  | ArithmeticN(n) -> Set.singleton (signOf n)
  | ArithmeticX(x) -> getResult x ds
  | ArithmeticFstR(x) -> getResult (x + ".fst") ds
  | ArithmeticSndR(x) -> getResult (x + ".snd") ds
  | ArithmeticA(x, a) -> match (mapArith a ds qs) with
                          | set when set |> Set.intersect (Set.ofList [Zero; Positive]) = Set.empty -> Set.empty
                          | _ -> getResult x ds
  | ROp(a1, op, a2) -> powerset (mapArith a1 ds qs) (mapArith a2 ds qs) op

let mapping (edge: Edge) (ds: DetectionOfSigns AnalysisAssignment): DetectionOfSigns Set =
  let (qs, action, qe) = edge
  match action with
  | ActionAssignmentL(l, a) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> 
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x) -> Set.singleton(DetectionOfSigns(x, Set.singleton(Zero)))
  | ActionAssignmentR(x, _, _) -> 
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> 
    | _ -> Set.empty
  | _ -> Set.empty


let analyse (pg: ProgramGraph) = 
  let (qs, _, _) = pg
  let domain : DetectionOfSigns AnalysisDomain = 
    {
      relation = Set.isSubset
      join = Set.union
      bottom = Set.empty
    }

  let spec : DetectionOfSigns AnalysisSpecification = 
    {
      domain = domain
      mapping = 
      initial = initial (ProgramGraph.variables pg)
    }

  analyseMonotone spec pg