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
    "examples/big_nested.mc"
    "examples/big_sequence.mc"
    "examples/big_if_sequence.mc";
    ]

  printfn "RD WITH FIFO"
  examples |> Seq.iter (run ReachingDefinitions.analyse (new Base.WorklistQueue<Node>()))
  printfn ""
  printfn "RD WITH LIFO"
  examples |> Seq.iter (run ReachingDefinitions.analyse (new Base.WorklistStack<Node>()))
  printfn ""
  printfn "RD WITH RPO"
  examples |> Seq.iter (runReversePostOrder ReachingDefinitions.analyse )
  printfn ""

  printfn "DS WITH FIFO"
  examples |> Seq.iter (run DetectionOfSigns.analyse (new Base.WorklistQueue<Node>()))
  printfn ""
  printfn "DS WITH LIFO"
  examples |> Seq.iter (run DetectionOfSigns.analyse (new Base.WorklistStack<Node>()))
  printfn ""

  printfn "DS WITH RPO"
  examples |> Seq.iter (runReversePostOrder DetectionOfSigns.analyse )
  printfn ""


  0
