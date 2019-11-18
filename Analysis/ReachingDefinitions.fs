module Analyses.ReachingDefinitions

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree

type ReachingDefinition = Node option * Node

let killset (action: Action) : ReachingDefinition Set option =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x) -> Some Set.empty
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x) -> Some Set.empty
    | _ -> None
  | ActionAssignmentR(r, _, _) -> Some Set.empty
  | ActionRead(a) -> 
    match a with
    | LabelX(x) -> Some Set.empty
    | _ -> None
  | _ -> None

let genset (edge: Edge) : ReachingDefinition Set =
  let (qs, action, qe) = edge
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> Set.singleton (ReachingDefinition(Some(qs), qe))
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x)
  | ActionAssignmentR(x, _, _) -> Set.singleton (ReachingDefinition(Some(qs), qe))
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> Set.singleton (ReachingDefinition(Some(qs), qe))
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

let killGenSetResult (edge: Edge) (rd: ReachingDefinition AnalysisAssignment): ReachingDefinition AnalysisMapping = 
  let (qs, action, qe) = edge
  let oldNodeMapping = rd.Item qs
  let newNodeMapping = rd.Item qe
  let kills = killset action
  let gens = genset edge
  
  let var = variableInAction action
  match var with
  | Some x -> match kills with
              | Some k -> let updatedMapping = k + gens
                          oldNodeMapping.Add(x, updatedMapping)
              | None -> let updatedMapping = oldNodeMapping.Item x + gens
                        oldNodeMapping.Add(x, updatedMapping)
  
  //let updatedMapping = (oldNodeMapping.Item x - kills) + gens
  //            newNodeMapping.Add(x, updatedMapping)
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


let analyse (pg: ProgramGraph) = 
  let (qs, _, _) = pg
  let domain : ReachingDefinition AnalysisDomain = 
    {
      relation = relation
      join = join
      bottom = bottom (variables pg)
    }

  let spec : ReachingDefinition AnalysisSpecification = 
    {
      domain = domain
      mapping = killGenSetResult
      initial = initial (variables pg) qs
    }

  analyseMonotone spec pg


//let cartesianNullable (x: string) (q: Node List): Set<ReachingDefinition> =
//  let l = None :: (q |> List.map Some)

//  let rec iterateEndingElem (x: string) (startingItem: Option<Node>) (endingItems: Node List): Set<ReachingDefinition> = 
//    match endingItems with
//    | head :: tail -> Set.union (Set.singleton (ReachingDefinition(x, startingItem, head))) (iterateEndingElem x startingItem tail)
//    | [] -> Set.empty

//  let rec iterateStartingElem (x: string) (startingItems: Option<Node> List) (endingItems: Node List) = 
//    match startingItems with
//    | head :: tail -> Set.union (iterateEndingElem x head endingItems) (iterateStartingElem x tail endingItems)
//    | [] -> Set.empty
  
  
//  iterateStartingElem x l q









//let updateKillGenSet (edge: Edge) (nodes: Node List) (rd: Map<Node, Set<ReachingDefinition>>): Map<Node, Set<ReachingDefinition>> =
//  let newSet = killGenSetResult edge nodes rd
//  let (_, _, qe) = edge
//  rd.Add(qe, newSet)


//let analyse (pg: ProgramGraph) = 
//  let init (xs: Set<string>) (qs: Node): Set<ReachingDefinition> =
//    Set.foldBack (fun x acc -> Set.add (ReachingDefinition(x, None, qs)) acc) xs Set.empty

//  let needsUpdating (edges: Edge List) (rd: Map<Node, ReachingDefinition Set>) (nodes: Node List) : bool =
//    List.exists (fun edge -> 
//      let res = killGenSetResult edge nodes rd
//      let (_, _, qe) = edge;
//      match rd.TryFind qe with
//      | Some(x) -> not (res.IsSubsetOf x)
//      | None -> false
//      ) edges

//  let updateAllEdges (edges: Edge List) (rd: Map<Node, ReachingDefinition Set>) (nodes: Node List) : Map<Node, ReachingDefinition Set> =
//    List.foldBack (fun edge acc -> updateKillGenSet edge nodes acc) edges rd

//  let (startnode, endnode, edges) = pg;
//  let nodeList = [startnode .. endnode];
//  let mutable rd =  ([startnode + 1 .. endnode] |> List.map (fun i -> i, Set.empty<ReachingDefinition>) |> Map.ofList)
  
//  rd <- rd.Add(startnode, init (variables pg) startnode)

//  // Algorithm here
//  while needsUpdating edges rd nodeList do
//    rd <- updateAllEdges edges rd nodeList

//  rd