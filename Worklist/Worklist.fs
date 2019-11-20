module Worklist.Implementation

open Worklist.Interface
open System.Collections.Generic

type WorklistQueue<'T> (elems : 'T seq) = 
  member internal this.queue = 
    let q = new Queue<'T>()
    for e in elems do
      q.Enqueue e
    q

  interface IWorklist<'T> with
    member this.Empty = 
      this.queue.Clear()
      this :> IWorklist<'T>

    member this.IsEmpty = this.queue.Count <= 0

    member this.Insert e = 
      this.queue.Enqueue e 
      this :> IWorklist<'T>

    member this.Extract = 
      let head = this.queue.Dequeue()
      (head, this :> IWorklist<'T>)

