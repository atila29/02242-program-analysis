module Worklist.Implementation

open ProgramGraph
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



type WorklistStack<'T> (elems : 'T seq) = 
  member internal this.stack = 
    let s = new Stack<'T>()
    for e in elems do
      s.Push e
    s

  interface IWorklist<'T> with
    member this.Empty = 
      this.stack.Clear()
      this :> IWorklist<'T>

    member this.IsEmpty = this.stack.Count <= 0

    member this.Insert e = 
      this.stack.Push e 
      this :> IWorklist<'T>

    member this.Extract = 
      let head = this.stack.Pop()
      (head, this :> IWorklist<'T>)

type AnalysisAssignment<'T when 'T : comparison> = Map<Node, 'T Set>

type AnalysisDomain<'T when 'T : comparison> = 
  {
    relation: 'T Set -> 'T Set -> bool
    join: 'T Set -> 'T Set -> 'T Set
    bottom: 'T Set
  }

type AnalysisSpecification<'T when 'T : comparison> = 
  {
    domain: 'T AnalysisDomain
    mapping: Edge -> 'T AnalysisAssignment -> 'T Set
    initial: 'T Set
  }

let analyzeWithQueue (spec: 'T AnalysisSpecification) (pg: ProgramGraph) : 'T AnalysisAssignment =
  let needsUpdating (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : bool =
    List.exists (fun edge -> 
      let newVal = spec.mapping edge resultSet
      let (_, _, qe) = edge
      not (spec.domain.relation newVal (resultSet.Item qe))
    ) edges

  let updateEdges (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : 'T AnalysisAssignment =
    List.foldBack (fun (qs, action, qe) acc -> 
      let newValue = spec.mapping (qs, action, qe) acc
      let oldValue = acc.Item qe
      acc.Add(qe, spec.domain.join oldValue newValue)
    ) edges resultSet

  let workList = new WorklistQueue<Node> nodes pg :> IWorklist;
  let (qs, qe, edges) = pg;
  let mutable resultSet = ([qs + 1 .. qe] |> List.map (fun i -> i, spec.domain.bottom) |> Map.ofList)
  resultSet <- resultSet.Add(qs, spec.initial)
  
  while not workList.IsEmpty do
    let (head, workList') = workList.Extract
    workList <- workList'
    for e in edges do
      let newVal = spec.mapping edge resultSet
      let (_, _, qe) = edge
      if (not spec.domain.relation newVal (resultSet.Item qe)) then
        workList.Insert qe

  resultSet  

let analyzeWithStack (spec: 'T AnalysisSpecification) (pg: ProgramGraph) : 'T AnalysisAssignment =
  let needsUpdating (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : bool =
    List.exists (fun edge -> 
      let newVal = spec.mapping edge resultSet
      let (_, _, qe) = edge
      not (spec.domain.relation newVal (resultSet.Item qe))
    ) edges

  let updateEdges (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : 'T AnalysisAssignment =
    List.foldBack (fun (qs, action, qe) acc -> 
      let newValue = spec.mapping (qs, action, qe) acc
      let oldValue = acc.Item qe
      acc.Add(qe, spec.domain.join oldValue newValue)
    ) edges resultSet

  let workList = new WorklistStack<Node> nodes pg :> IWorklist;
  let (qs, qe, edges) = pg;
  let mutable resultSet = ([qs + 1 .. qe] |> List.map (fun i -> i, spec.domain.bottom) |> Map.ofList)
  resultSet <- resultSet.Add(qs, spec.initial)
  
  while not workList.IsEmpty do
    let (head, workList') = workList.Extract
    workList <- workList'
    for e in edges do
      let newVal = spec.mapping edge resultSet
      let (_, _, qe) = edge
      if (not spec.domain.relation newVal (resultSet.Item qe)) then
        workList.Insert qe

  resultSet 