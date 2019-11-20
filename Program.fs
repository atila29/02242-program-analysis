// Learn more about F# at http://fsharp.org
open ParsingUtil
open ProgramGraph
open Analyses
open AbstractSyntaxTree

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
        x := 2*2;
        R := (1, 10);

        if (x == 4)
        {
            R.fst := x;
            x := R.snd;
        }
    }"

    let graph2 = convertToProgramGraph (parseString test_2_3)

    printfn "%A" graph2

    printf "%A" (ReachingDefinitions.analyse graph2)

    // Exit code
    0
