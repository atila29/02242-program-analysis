open ParsingUtil
open ProgramGraph
open Analyses
open Worklist.Implementation
open Example.Runner



[<EntryPoint>]
let main _ =

  let examples = [
    "examples/test2.mc";
    "examples/simple.mc"; 
    "examples/if.mc"; 
    "examples/ifelse.mc" ; 
    "examples/loop.mc"; 
    "examples/nested.mc"; 
    "examples/nestedloops.mc"]

  examples |> Seq.iter (run ReachingDefinitions.analyse (new Base.WorklistQueue<Node>()))
  examples |> Seq.iter (run ReachingDefinitions.analyse (new Base.WorklistStack<Node>()))

  examples |> Seq.iter (run DetectionOfSigns.analyse (new Base.WorklistQueue<Node>()))
  examples |> Seq.iter (run DetectionOfSigns.analyse (new Base.WorklistStack<Node>()))


  0
