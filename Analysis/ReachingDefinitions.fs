module Analyses.ReachingDefinitions

open ProgramGraph

// RD(1) = {}

type ReachingDefintion = string * Node option * Node

let analyse (pg: ProgramGraph) () = 
  List.empty
  // let (startnode, endnode, edges) = pg;
  // // let rd = List.Init endnode Set<ReachingDefintion>

  // // Algorithm here
  // let rec loop (edges: Edge List) (rd: Map<Node, Set<ReachingDefintion>>) =
  //   match edges with
  //     | head :: tail -> loop tail (updatekillgensets head)
  //     | [] -> rd

  // loop edges Map.empty