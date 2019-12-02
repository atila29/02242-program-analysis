module Analyses.DetectionOfSigns

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree

type Sign = Negative | Zero | Positive
and DetectionOfSigns =  Sign


let initial (xs: string Set) : DetectionOfSigns AnalysisMapping =
  xs |> Set.toSeq |> Seq.map(fun x -> (x, Set.ofList[Negative; Zero; Positive])) |> Map.ofSeq

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

let negateArith (s: Sign Set) : Sign Set = 
  Set.foldBack (fun e acc -> acc + times Negative e) s Set.empty

let rec mapArith (arith: A) (ds: DetectionOfSigns AnalysisMapping) =
  match arith with
  | ArithmeticN(n) -> Set.singleton (signOf n)
  | ArithmeticX(x) -> Map.find x ds
  | ArithmeticFstR(x) -> Map.find (x + ".fst") ds
  | ArithmeticSndR(x) -> Map.find (x + ".snd") ds
  | ArithmeticA(x, a) -> match (mapArith a ds) with
                          | set when set |> Set.intersect (Set.ofList [Zero; Positive]) = Set.empty -> Set.empty
                          | _ -> Map.find x ds
  | ROp(a1, op, a2) -> powerset (mapArith a1 ds) (mapArith a2 ds) op
  | ArithmeticNeg(a) -> negateArith (mapArith a ds)

let bot: Sign Set = Set.empty

// While this is not entirely a functional approach
// it just makes propegating an error that much easier
let mutable globalFail = false;
let failedbefore(): bool =
  globalFail
let propegateFail(): DetectionOfSigns AnalysisMapping =
  globalFail <- true
  Map.empty

let analysisfunction (edge: Edge) (ds: DetectionOfSigns AnalysisMapping): DetectionOfSigns AnalysisMapping =
  // We can only update the next value if our analysisassignment actually contains mappings
  if ds.IsEmpty then
    Map.empty
  else
    let (_, action, _) = edge

    match action with
    | ActionDeclarationR(x) -> ds.Add(x + ".fst", Set.singleton Zero).Add(x + ".snd", Set.singleton Zero) 
    | ActionDeclarationX(x)
    | ActionDeclarationA(x, _) -> ds.Add(x, Set.singleton Zero)
    | ActionAssignmentL(l, a) -> 
      match l with
      | LabelX(x) ->  match (mapArith a ds) with
                      | signs when signs <> Set.empty && not (failedbefore()) -> ds.Add(x, signs)
                      | _ -> propegateFail()
      | LabelA(x, a') -> match ((mapArith a' ds), (mapArith a ds)) with  
                          |(aIndex, signs) when aIndex |> Set.intersect (Set.ofList [Zero; Positive]) <> Set.empty && signs <> Set.empty && not (failedbefore()) -> ds.Add(x, signs)
                          | _ ->  propegateFail()
      | LabelFstR(x) -> match (mapArith a ds) with
                        | signs when signs <> Set.empty && not (failedbefore()) -> ds.Add(x + ".fst", signs)
                        | _ -> propegateFail()
      | LabelSndR(x) -> match (mapArith a ds) with
                        | signs when signs <> Set.empty && not (failedbefore()) -> ds.Add(x + ".snd", signs)
                        | _ -> propegateFail()
    | ActionAssignmentR(x, a1, a2) -> match ((mapArith a1 ds), (mapArith a2 ds)) with
                                      | (signs1, signs2) when signs1 <> Set.empty && signs2 <> Set.empty && not (failedbefore()) -> ds.Add(x + ".fst", signs1).Add(x + ".snd", signs2)
                                      | _ -> propegateFail()
    | ActionRead(l) -> 
      match l with
      | LabelA(x, a) -> match (mapArith a ds) with
                        | signs when signs |> Set.intersect (Set.ofList [Zero; Positive]) = Set.empty && not (failedbefore()) -> ds.Add(x, signs)
                        | _ -> propegateFail()
      | LabelX(x) -> if not (failedbefore()) then
                      ds.Add(x, Set.ofList [Negative; Zero; Positive])
                     else 
                      propegateFail()
      | LabelFstR(x) -> if not (failedbefore()) then
                          ds.Add(x + ".fst", Set.ofList [Negative; Zero; Positive])
                        else 
                          propegateFail()
      | LabelSndR(x) -> if not (failedbefore()) then
                          ds.Add(x + ".snd", Set.ofList [Negative; Zero; Positive])
                        else 
                          propegateFail()
    | _ -> ds

let tryGetOrWithValue (map: Map<string, 'T Set>) (key: string) (defaultReturn: 'T Set) =
  match map.TryFind key with
  | Some x -> x
  | None -> defaultReturn

let ordering (t1: DetectionOfSigns AnalysisMapping) (t2: DetectionOfSigns AnalysisMapping) : bool =
  //Map.forall (fun key value -> Set.isSubset value (t2.Item key) ) t1
  Map.forall (fun key value -> Set.isSubset value (tryGetOrWithValue t2 key Set.empty)) t1

let join (t1: DetectionOfSigns AnalysisMapping) (t2: DetectionOfSigns AnalysisMapping) : DetectionOfSigns AnalysisMapping =
  Map.fold (fun acc key value -> acc.Add(key, (tryGetOrWithValue acc key Set.empty + value))) t1 t2

let analyse (pg: ProgramGraph) = 
  let domain : DetectionOfSigns AnalysisDomain = 
    {
      ordering = ordering
      join = join
      bottom = Map.empty
    }

  let spec : DetectionOfSigns AnalysisSpecification = 
    {
      domain = domain
      analysisfunction = analysisfunction
      initial = initial (ProgramGraph.variables pg)
    }

  analyseMonotone spec pg