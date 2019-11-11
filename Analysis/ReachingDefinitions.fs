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

let killset (action: Action) (bigQ: Node List): Set<ReachingDefintion> =
  match action with
  | ActionAssignmentL(l, _) -> match l with
                                | LabelX(x) -> cartesianNullable x bigQ
                                | _ -> Set.empty
  | ActionAssignmentR(r, _, _) -> cartesianNullable r bigQ
  | ActionRead(a) -> match a with
                      | LabelX(x) -> cartesianNullable x bigQ
                      | _ -> Set.empty
  | _ -> Set.empty

let genset (qs: Node) (action: Action) (qe: Node): Set<ReachingDefintion> =
  match action with
  | ActionAssignmentL(l, _) -> match l with
                                | LabelX(x)
                                | LabelA(x, _)
                                | LabelFstR(x)
                                | LabelSndR(x) -> Set.singleton (ReachingDefintion(x, Some(qs), qe))
  | ActionAssignmentR(r, _, _) -> Set.singleton (ReachingDefintion(r, Some(qs), qe))
  | ActionRead(l) -> match l with
                      | LabelX(x)
                      | LabelA(x, _) -> Set.singleton (ReachingDefintion(x, Some(qs), qe))
                      | _ -> Set.empty
  | _ -> Set.empty


// Todo: This will probably be needed elsewhere, consider moving
//let rec getVariables (edges: Edge List) (set: Set<string>): Set<string> =
//  match edges with
//  | (_, action, _) :: tail -> Set.union (getVariables tail set) 
//                                (match action with
//                                 | ActionDeclarationX(x)
//                                 | ActionDeclarationA(x, _)
//                                 | ActionDeclarationR(x) -> Set.singleton x
//                                 | ActionAssignmentL(l, a) -> match l with
//                                                                | LabelX(x)
//                                                                | LabelA(x)
//                                                                | LabelFstR(x)
//                                                                | LabelSndR(x) -> Set.union (Set.singleton x) (match a with
//                                                                                                               | 
//                                                                                                               )                                 
//                                 )
//  | [] -> set

let updateKillGenSet (edge: Edge) (rd: Map<Node, Set<ReachingDefintion>>): Map<Node, Set<ReachingDefintion>> =
  let (qs, action, qe) = edge
  let kills = killset action [qs .. qe]
  let gens = genset qs action qe

  let newSet = Set.union (Set.difference (rd.Item qe) kills) gens
  rd.Add(qe, newSet)


let analyse (pg: ProgramGraph) = 
  let (startnode, endnode, edges) = pg;
  let rd =  [startnode + 1 .. endnode] |> List.map (fun i -> i, Set.empty<ReachingDefintion>) |> Map.ofList
  
  // Todo: Need to add all the variables to the starting node
  // rd.Add(startnode, ??)

  // Algorithm here
  let rec loop (edges: Edge List) (rd: Map<Node, Set<ReachingDefintion>>) =
    match edges with
      | head :: tail -> loop tail (updateKillGenSet head rd)
      | [] -> rd

  loop edges rd