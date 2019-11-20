module Analyses.ReachingDefinitions

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree

type ReachingDefinition = string * Node option * Node



let cartesianNullable (x: string) (q: Node List): ReachingDefinition Set =
  let l = None :: (q |> List.map Some)

  let rec iterateEndingElem (x: string) (startingItem: Option<Node>) (endingItems: Node List): Set<ReachingDefinition> = 
    match endingItems with
    | head :: tail -> Set.union (Set.singleton (ReachingDefinition(x, startingItem, head))) (iterateEndingElem x startingItem tail)
    | [] -> Set.empty

  let rec iterateStartingElem (x: string) (startingItems: Option<Node> List) (endingItems: Node List) = 
    match startingItems with
    | head :: tail -> Set.union (iterateEndingElem x head endingItems) (iterateStartingElem x tail endingItems)
    | [] -> Set.empty
  
  
  iterateStartingElem x l q


let killset (action: Action) (nodes: Node List) : ReachingDefinition Set =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x)
  | ActionAssignmentR(x, _, _) -> cartesianNullable x nodes
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x) -> cartesianNullable x nodes
    | _ -> Set.empty
  | ActionRead(a) -> 
    match a with
    | LabelX(x) -> cartesianNullable x nodes
    | _ -> Set.empty
  | _ -> Set.empty

let genset (edge: Edge) : ReachingDefinition Set =
  let (qs, action, qe) = edge
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> Set.singleton (ReachingDefinition(x, Some(qs), qe))
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x)
  | ActionAssignmentR(x, _, _) -> Set.singleton (ReachingDefinition(x, Some(qs), qe))
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> Set.singleton (ReachingDefinition(x, Some(qs), qe))
    | _ -> Set.empty
  | _ -> Set.empty

let variableInAction (action: Action) : string option =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x)
  | ActionAssignmentR(x, _, _) -> Some x
  | ActionRead(x)
  | ActionAssignmentL(x, _) -> match x with
                                | LabelX(x)
                                | LabelA(x, _)
                                | LabelFstR(x)
                                | LabelSndR(x) -> Some x
  | _ -> None

let killGenSetResult (edge: Edge) (rd: ReachingDefinition AnalysisAssignment) (nodes: Node List): ReachingDefinition Set = 
  let (qs, action, qe) = edge
  let kills = killset action nodes
  let gens = genset edge
  rd.Item qs - kills + gens

let killGetSetWrapper (nodes: Node List) =
  fun e rd -> killGenSetResult e rd nodes


let initial (xs: string Set) (qs: Node) : ReachingDefinition Set =
  Set.map (fun x -> ReachingDefinition(x, None, qs)) xs


let analyse (pg: ProgramGraph) = 
  let (qs, _, _) = pg
  let domain : ReachingDefinition AnalysisDomain = 
    {
      relation = Set.isSubset
      join = Set.union
      bottom = Set.empty
    }

  let spec : ReachingDefinition AnalysisSpecification = 
    {
      domain = domain
      mapping = killGetSetWrapper (ProgramGraph.nodes pg)
      initial = initial (ProgramGraph.variables pg) qs
    }

  analyseMonotone spec pg