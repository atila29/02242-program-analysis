// Learn more about F# at http://fsharp.org
open ParsingUtil
open ProgramGraph
open Analyses
open AbstractSyntaxTree

[<EntryPoint>]
let main argv =
    let x ="
    {
       int[5] a;
       int x;
        
       x := 4;
       a[x] := 2;
       if (a[3] == 12) {
           x := a[x];
       }
    }"

    let tokens = parseString x
    let graph = convertToProgramGraph tokens

    printfn "%A" graph

    let idk = ReachingDefinitions.analyse graph

    printfn "%A" idk

    printfn "set breakpoint to see ast value"



    let test_2_3 = "
    {
        {int fst; int snd} r;
        int x;
        x := 2*2;
        r := (1, 10);

        if (x == 4)
        {
            r.fst := x;
            x := R.snd;
        }
    }"

    printf "%A" (ReachingDefinitions.analyse (convertToProgramGraph (parseString test_2_3)))


    // let labelTest = ActionBool(Not(BoolValue(true)))

    // printfn labelTest

    // Exit code
    0
