open ParsingUtil
open ProgramGraph
open Analyses
open Worklist.Implementation

[<EntryPoint>]
let main _ =
    let input = "
    {
        {int fst; int snd} R;
        int x;
        int[5] A;
        x := 2*2;
        R := (1, -10);

        if (x == 4)
        {
            R.fst := x;
            x := R.snd;
        }
        else {
          R.snd := 15;
        }
        x := 5;
    }"


    let graph = convertToProgramGraph (parseString input)
    
    let (qs, qe, edges) = graph;
    printfn "ProgramGraph:"
    printfn "qs: %d, qe: %d" qs qe
    edges |> Seq.iter (printfn "%A")

    printfn ""

    let worklist = new WorklistQueue<Node>()
    let result = DetectionOfSigns.analyse graph worklist

    printfn "AnalysisAssigenment:"
    result |> Seq.iter (fun x ->  printf "%d - " x.Key 
                                  Seq.iter (fun x' -> printf "%A " x') x.Value
                                  printfn "")

    // Exit code
    0
