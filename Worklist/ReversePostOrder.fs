module Worklist.Implementation.ReversePostOrder

open Worklist.Interface
open ProgramGraph

let DFS (pg: ProgramGraph) =

  let mutable T = Set.empty
  let mutable V = Set.empty
  let mutable rP = Map.empty

  let (start, _k, edges) = pg
  let mutable k = _k + 1


  let rec procedure (node: Node) =
    V <- Set.add node V

    edges 
      |> Seq.filter (fun (qs, _, qe) -> qs = node && not (Set.contains qe V))  
      |> Seq.iter (fun (qs, _, qe) -> 
        T <- Set.add (qs, qe) T
        procedure qe
        )

    rP <- rP.Add(node, k)
    k <- k - 1
  
  procedure start

  (T, rP)


type Worklist<'T when 'T : comparison>(v: 'T List, p: 'T Set, rP: Map<'T, int>) = 
  member this.V = v
  member this.P = p
  member this.Rp = rP

  interface IWorklist<'T> with
    member this.Empty = Worklist<_>(List.empty, Set.empty, this.Rp) :> IWorklist<'T>

    member this.IsEmpty = this.V.IsEmpty && this.P.IsEmpty

    member this.Insert q = if not (List.contains q this.V) then
                            Worklist<_>(this.V, Set.add q this.P, this.Rp) :> IWorklist<'T>
                           else
                            Worklist<_>(this.V, this.P, this.Rp) :> IWorklist<'T>

    member this.Extract = match this.V with
                          | q :: tail -> (q, Worklist(tail, this.P, this.Rp) :> IWorklist<'T>) 
                          | _ -> let v' = this.P |> Set.toList |> List.sortBy (fun x -> this.Rp.Item x)
                                 (v'.Head, Worklist(v'.Tail, Set.empty, this.Rp) :> IWorklist<'T>)
    

