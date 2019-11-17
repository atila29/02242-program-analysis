module Analyses.ReachingDefinitions

open ProgramGraph
open AbstractSyntaxTree


type ReachingDefinition = string * Node option * Node

let cartesianNullable (x: string) (q: Node List): Set<ReachingDefinition> =
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



let killset (action: Action) (bigQ: Node List): Set<ReachingDefinition> =
  match action with
  | ActionDeclarationX(x)
  | ActionDeclarationA(x, _)
  | ActionDeclarationR(x) -> cartesianNullable x bigQ
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x) -> cartesianNullable x bigQ
    | _ -> Set.empty
  | ActionAssignmentR(r, _, _) -> cartesianNullable r bigQ
  | ActionRead(a) -> 
    match a with
    | LabelX(x) -> cartesianNullable x bigQ
    | _ -> Set.empty
  | _ -> Set.empty

let genset (qs: Node) (action: Action) (qe: Node): Set<ReachingDefinition> =
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> Set.singleton (ReachingDefinition(x, Some(qs), qe))
  | ActionAssignmentR(r, _, _) -> Set.singleton (ReachingDefinition(r, Some(qs), qe))
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> Set.singleton (ReachingDefinition(x, Some(qs), qe))
    | _ -> Set.empty
  | _ -> Set.empty


let killGenSetResult (edge: Edge) (nodes: Node List) (rd: Map<Node, ReachingDefinition Set>): ReachingDefinition Set = 
  let (qs, action, qe) = edge
  let kills = killset action nodes
  let gens = genset qs action qe

  rd.Item qe + (rd.Item qs - kills) + gens


let updateKillGenSet (edge: Edge) (nodes: Node List) (rd: Map<Node, Set<ReachingDefinition>>): Map<Node, Set<ReachingDefinition>> =
  let newSet = killGenSetResult edge nodes rd
  let (_, _, qe) = edge
  rd.Add(qe, newSet)


let analyse (pg: ProgramGraph) = 
  let init (xs: Set<string>) (qs: Node): Set<ReachingDefinition> =
    Set.foldBack (fun x acc -> Set.add (ReachingDefinition(x, None, qs)) acc) xs Set.empty

  let needsUpdating (edges: Edge List) (rd: Map<Node, ReachingDefinition Set>) (nodes: Node List) : bool =
    List.exists (fun edge -> 
      let res = killGenSetResult edge nodes rd
      let (_, _, qe) = edge;
      match rd.TryFind qe with
      | Some(x) -> not (res.IsSubsetOf x)
      | None -> false
      ) edges

  let updateAllEdges (edges: Edge List) (rd: Map<Node, ReachingDefinition Set>) (nodes: Node List) : Map<Node, ReachingDefinition Set> =
    List.foldBack (fun edge acc -> updateKillGenSet edge nodes acc) edges rd

  let (startnode, endnode, edges) = pg;
  let nodeList = [startnode .. endnode];
  let mutable rd =  ([startnode + 1 .. endnode] |> List.map (fun i -> i, Set.empty<ReachingDefinition>) |> Map.ofList)
  
  rd <- rd.Add(startnode, init (variables pg) startnode)

  // Algorithm here
  while needsUpdating edges rd nodeList do
    rd <- updateAllEdges edges rd nodeList

  rd



  //let rec loop (edges: Edge List) (rd: Map<Node, Set<ReachingDefinition>>) =
  //  match edges with
  //    | head :: tail -> loop tail (updateKillGenSet head nodeList rd)
  //    | [] -> rd

  //loop edges rd