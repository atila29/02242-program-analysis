open ParsingUtil
open ProgramGraph
open Analyses
open Worklist.Implementation

[<EntryPoint>]
let main argv =
    
    //let x ="
    //{
    //   int[5] a;
    //   int x;
        
    //   x := 4;
    //   a[x] := 2;
    //   if (a[3] == 12) {
    //       x := a[x];
    //   }
    //}"

    //let tokens = parseString x
    //let graph = convertToProgramGraph tokens

    //printfn "%A" graph

    //let idk = ReachingDefinitions.analyse graph

    //printfn "%A" idk

    //printfn "set breakpoint to see ast value"



    let test_2_3 = "
    {
        {int fst; int snd} R;
        int x;
        int[5] A;
        x := 2*2;
        R := (1, -10);
        A[-1] := 5;

        if (x == 4)
        {
            R.fst := x;
            x := R.snd;
        }
    }"

    let graph2 = convertToProgramGraph (parseString test_2_3)

    printfn "ProgramGraph:\n%A" graph2

    let worklist = new WorklistQueue<Node>()

    printf "AnalysisAssigenment:\n%A" (DetectionOfSigns.analyse graph2 worklist)

    // Exit code
    0
