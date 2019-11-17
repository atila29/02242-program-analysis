module Analyses.ReachingDefinitions

open ProgramGraph
open MonotoneFramework
open AbstractSyntaxTree

type ReachingDefinition = Node option * Node

let killset (action: Action) (mapping: ReachingDefinition AnalysisMapping) : ReachingDefinition AnalysisMapping =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x) -> mapping.Add(x, Set.empty)
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x) -> mapping.Add(x, Set.empty)
    | _ -> mapping
  | ActionAssignmentR(r, _, _) -> mapping.Add(r, Set.empty)
  | ActionRead(a) -> 
    match a with
    | LabelX(x) -> mapping.Add(x, Set.empty)
    | _ -> mapping
  | _ -> mapping

let genset (edge: Edge) (mapping: ReachingDefinition AnalysisMapping): ReachingDefinition AnalysisMapping =
  let (qs, action, qe) = edge
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> mapping.Add(x, Set.singleton (ReachingDefinition(Some(qs), qe)))
  | ActionAssignmentR(r, _, _) -> mapping.Add(r, Set.singleton (ReachingDefinition(Some(qs), qe)))
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> mapping.Add(x, Set.singleton (ReachingDefinition(Some(qs), qe)))
    | _ -> mapping
  | _ -> mapping


let killGenSetResult (edge: Edge) (rd: ReachingDefinition AnalysisAssignment): ReachingDefinition AnalysisMapping = 
  let (qs, action, qe) = edge
  let oldNodeMapping = rd.Item qs
  let newNodeMapping = rd.Item qe
  let kills = killset action oldNodeMapping
  let gens = genset edge newNodeMapping
  
  let x = ""

  let updatedMapping = (oldNodeMapping.Item x - kills.Item x) + gens.Item x 
  newNodeMapping.Add(x, updatedMapping)

let analyse (pg: ProgramGraph) = 
  let domain : ReachingDefinition AnalysisDomain = 
    {
      relation = Set.isSubset
      join = Set.union
      bottom = Set.empty
    }

  let spec : ReachingDefinition AnalysisSpecification = 
    {
      domain = domain
      mapping = killGenSetResult
      initial = Set.empty
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