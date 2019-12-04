module Worklist.Implementation.Base

open Worklist.Interface
open System.Collections.Generic

type WorklistQueue<'T>() = 
  let q = new Queue<'T>()
  member internal _this.queue = q

  interface IWorklist<'T> with
    member this.Empty = 
      this.queue.Clear()
      this :> IWorklist<'T>

    member this.IsEmpty = this.queue.Count <= 0

    member this.Insert e =
      if(not (this.queue.Contains e)) then
        this.queue.Enqueue e
      this :> IWorklist<'T>

    member this.Extract = 
      let head = this.queue.Dequeue()
      (head, this :> IWorklist<'T>)



type WorklistStack<'T> () = 
  let s = new Stack<'T>()
  member internal this.stack = s

  interface IWorklist<'T> with
    member this.Empty = 
      this.stack.Clear()
      this :> IWorklist<'T>

    member this.IsEmpty = this.stack.Count <= 0

    member this.Insert e =
      if (not (this.stack.Contains e)) then
        this.stack.Push e 
      this :> IWorklist<'T>

    member this.Extract = 
      let head = this.stack.Pop()
      (head, this :> IWorklist<'T>)