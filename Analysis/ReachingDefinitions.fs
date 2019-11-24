module Analyses.ReachingDefinitions

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree
open Worklist.Interface

type ReachingDefinition = Node option * Node



let killset (action: Action): ReachingDefinition Set option =
  match action with
  | ActionDeclarationX(_)
  | ActionDeclarationA(_, _)
  | ActionDeclarationR(_)
  | ActionAssignmentR(_, _, _) -> Some Set.empty
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(_) 
    | LabelFstR(_)
    | LabelSndR(_) -> Some Set.empty
    | _ -> None
  | ActionRead(a) -> 
    match a with
    | LabelX(_)
    | LabelFstR(_)
    | LabelSndR(_) -> Some Set.empty
    | _ -> None
  | _ -> None

let genset (edge: Edge) : ReachingDefinition Set =
  let (qs, action, qe) = edge
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(_)
    | LabelA(_, _)
    | LabelFstR(_)
    | LabelSndR(_) -> Set.singleton (ReachingDefinition(Some(qs), qe))
  | ActionDeclarationX(_)
  | ActionDeclarationA(_, _)
  | ActionDeclarationR(_)
  | ActionAssignmentR(_, _, _) -> Set.singleton (ReachingDefinition(Some(qs), qe))
  | ActionRead(l) -> 
    match l with
    | LabelX(_)
    | LabelA(_, _) -> Set.singleton (ReachingDefinition(Some(qs), qe))
    | _ -> Set.empty
  | _ -> Set.empty

let updateMapping (rdmapping: ReachingDefinition AnalysisMapping) (kills: ReachingDefinition Set option) (gens: ReachingDefinition Set) (x: string): ReachingDefinition AnalysisMapping =
  match kills with
  | Some k -> let updatedMapping = k + gens
              rdmapping.Add(x, updatedMapping)
  | None -> let updatedMapping = rdmapping.Item x + gens
            rdmapping.Add(x, updatedMapping)

let killGenSetResult (edge: Edge) (rd: ReachingDefinition AnalysisAssignment): ReachingDefinition AnalysisMapping = 
  let (qs, action, _) = edge
  let oldNodeMapping = rd.Item qs
  let kills = killset action
  let gens = genset edge

  let var = ProgramGraph.variableInAction action
  match var with
  | Some xs -> Set.foldBack (fun x acc -> updateMapping acc kills gens x) xs oldNodeMapping
  | None -> oldNodeMapping

// These probably need to be made into some sort of common module, because
// they will be used in more implementations
let relation (t1: ReachingDefinition AnalysisMapping) (t2: ReachingDefinition AnalysisMapping) : bool =
  Map.forall (fun key value -> Set.isSubset value (t2.Item key) ) t1

let join (t1: ReachingDefinition AnalysisMapping) (t2: ReachingDefinition AnalysisMapping) : ReachingDefinition AnalysisMapping =
  // Good luck
  Map.fold (fun acc key value -> acc.Add(key, (acc.Item key + value))) t1 t2

let bottom (xs: string Set) : ReachingDefinition AnalysisMapping = 
  xs |> Set.toSeq |> Seq.map (fun x -> (x, Set.empty)) |> Map.ofSeq

let initial (xs: string Set) (qs: Node) : ReachingDefinition AnalysisMapping =
  xs |> Set.toSeq |> Seq.map (fun x -> (x, Set.singleton (ReachingDefinition(None, qs)))) |> Map.ofSeq


let analyse (pg: ProgramGraph) (worklist: Node IWorklist) = 
  let (qs, _, _) = pg
  let domain : ReachingDefinition AnalysisDomain = 
    {
      relation = relation
      join = join
      bottom = bottom (ProgramGraph.variables pg)
    }

  let spec : ReachingDefinition AnalysisSpecification = 
    {
      domain = domain
      mapping = killGenSetResult
      initial = initial (ProgramGraph.variables pg) qs
    }

  analyseMonotone spec pg worklist