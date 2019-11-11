module Analyses.ReachingDefinitions

open ProgramGraph
open AbstractSyntaxTree


type ReachingDefintion = string * Node option * Node

let cartesianNullable (x: string) (q: Node List): Set<ReachingDefintion> =
  let l = None :: (q |> List.map (fun i -> Some i))

  let rec iterateEndingElem (x: string) (startingItem: Option<Node>) (endingItems: Node List): Set<ReachingDefintion> = 
    match endingItems with
    | head :: tail -> Set.union (Set.singleton (ReachingDefintion(x, startingItem, head))) (iterateEndingElem x startingItem tail)
    | [] -> Set.empty

  let rec iterateStartingElem (x: string) (startingItems: Option<Node> List) (endingItems: Node List) = 
    match startingItems with
    | head :: tail -> Set.union (iterateEndingElem x head endingItems) (iterateStartingElem x tail endingItems)
    | [] -> Set.empty
  
  
  iterateStartingElem x l q


let cartesianNullableList (xs: Set<string>) (q: Node List): Set<ReachingDefintion> =
  Set.foldBack (fun x acc -> acc + cartesianNullable x q) xs Set.empty


let killset (action: Action) (bigQ: Node List): Set<ReachingDefintion> =
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

let genset (qs: Node) (action: Action) (qe: Node): Set<ReachingDefintion> =
  match action with
  | ActionAssignmentL(l, _) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _)
    | LabelFstR(x)
    | LabelSndR(x) -> Set.singleton (ReachingDefintion(x, Some(qs), qe))
  | ActionAssignmentR(r, _, _) -> Set.singleton (ReachingDefintion(r, Some(qs), qe))
  | ActionRead(l) -> 
    match l with
    | LabelX(x)
    | LabelA(x, _) -> Set.singleton (ReachingDefintion(x, Some(qs), qe))
    | _ -> Set.empty
  | _ -> Set.empty



let updateKillGenSet (edge: Edge) (bigQ: Node List) (rd: Map<Node, Set<ReachingDefintion>>): Map<Node, Set<ReachingDefintion>> =
  let (qs, action, qe) = edge
  let kills = killset action bigQ
  let gens = genset qs action qe

  let newSet = Set.union (Set.difference (rd.Item qe) kills) gens
  rd.Add(qe, newSet)


let analyse (pg: ProgramGraph) = 
  let (startnode, endnode, edges) = pg;
  let nodeList = [startnode .. endnode];
  let rd =  ([startnode + 1 .. endnode] |> List.map (fun i -> i, Set.empty<ReachingDefintion>) |> Map.ofList)
  
  // Todo: Need to find a better way to add this default element...
  let rd = rd.Add(startnode, cartesianNullableList (variables pg) [startnode .. endnode])

  // Algorithm here
  let rec loop (edges: Edge List) (rd: Map<Node, Set<ReachingDefintion>>) =
    match edges with
      | head :: tail -> loop tail (updateKillGenSet head nodeList rd)
      | [] -> rd

  loop edges rd