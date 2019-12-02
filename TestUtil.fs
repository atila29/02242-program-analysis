module Example.Runner

open ParsingUtil
open ProgramGraph
open Worklist.Interface
open MonotoneFramework
open Worklist.Implementation


let run (anaylysis: ProgramGraph -> IWorklist<Node> -> AnalysisAssignment<'T>) worklist filename =  
  printfn "\nexecution of %s:" filename 
  let ast = parseFromFile filename
  let graph = convertToProgramGraph ast
  let (qs, qe, edges) = graph;
  // printfn "ProgramGraph:"
  // printfn "qs: %d, qe: %d" qs qe
  // edges |> Seq.iter (printfn "%A")
  printfn "vizgraph:"
  // printfn "%s" (printVizGraph graph)
  let result = anaylysis graph worklist
  // printfn "AnalysisAssigenment:"
  // result |> Seq.iter (fun x ->  printf "%d - " x.Key 
  //                               Seq.iter (fun x' -> 
  //                                               let (key, value) = ``|KeyValue|`` x'
  //                                               printf "%s [" key
  //                                               value |> Seq.iter (fun x'' -> printf "%A; " x'')
  //                                        ) x.Value
  //                               printfn "]")
  printf ""

let runReversePostOrder (anaylysis: ProgramGraph -> IWorklist<Node> -> AnalysisAssignment<'T>) filename =
  printfn "\nexecution of %s:" filename 
  let ast = parseFromFile filename
  let graph = convertToProgramGraph ast
  let (T, rp) = ReversePostOrder.DFS graph                                       
  let (qs, qe, edges) = graph;
  // printfn "ProgramGraph:"
  // printfn "qs: %d, qe: %d" qs qe
  // edges |> Seq.iter (printfn "%A")
  printfn "vizgraph:"
  // printfn "%s" (printVizGraph graph)
  let result = anaylysis graph (new ReversePostOrder.Worklist<Node>(List.empty, Set.empty, rp))
  // printfn "AnalysisAssigenment:"
  // result |> Seq.iter (fun x ->  printf "%d - " x.Key 
  //                               Seq.iter (fun x' -> 
  //                                               let (key, value) = ``|KeyValue|`` x'
  //                                               printf "%s [" key
  //                                               value |> Seq.iter (fun x'' -> printf "%A; " x'')
  //                                        ) x.Value
  //                               printfn "]")
  printf ""


