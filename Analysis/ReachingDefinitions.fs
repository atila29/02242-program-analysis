module Analyses.ReachingDefinitions

open ProgramGraph
open AbstractSyntaxTree


type ReachingDefintion = string * Node option * Node


let killset (action: Action) (bigQ: Node List): Set<ReachingDefintion> =
  match action with
  | ActionAssignmentL(l, _) -> match l with
                                | LabelX(x) ->  cartesian (cartesian x None) bigQ  //ReachingDefintion(x, Some(qs), qe)
                                | _ -> Set.empty
  | ActionAssignmentR(r, _, _) -> ReachingDefintion(r, Some(qs) ,qe)
  | ActionRead(a) -> match a with
                      | LabelX(x) -> ReachingDefintion(x, Some(qs), qe)
                      | _ -> Set.empty
  | _ -> Set.empty

let genset (qs: Node) (action: Action) (qe: Node): Set<ReachingDefintion> =
  match action with
  | ActionAssignmentL(l, _) -> match l with
                                | LabelX(x)
                                | LabelA(x, _)
                                | LabelFstR(x)
                                | LabelSndR(x) -> Set.singleton ReachingDefintion(x, Some(qs), qe)
  | ActionAssignmentR(r, _, _) -> Set.singleton ReachingDefintion(r, Some(qs), qe)
  | ActionRead(l) -> match l with
                      | LabelX(x)
                      | LabelA(x, _) -> Set.singleton ReachingDefintion(x, Some(qs), qe)
                      | _ -> Set.empty
  | _ -> Set.empty

let constraintSet (edge: Edge) (rd: Map<Node, Set<ReachingDefintion>>) =
  let (qs, action, qe) = edge
  let kills = killset action

  List.empty

let analyse (pg: ProgramGraph) () = 
  let (startnode, endnode, edges) = pg;
  // let rd = List.Init endnode Set<ReachingDefintion>

  // Algorithm here
  let rec loop (edges: Edge List) (rd: Map<Node, Set<ReachingDefintion>>) =
    match edges with
      | head :: tail -> loop tail (updatekillgensets head rd)
      | [] -> rd

  loop edges Map.empty