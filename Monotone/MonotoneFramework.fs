module MonotoneFramework

open ProgramGraph
open Worklist.Interface

type AnalysisMapping<'T when 'T : comparison> = Map<string, 'T Set>
and AnalysisAssignment<'T when 'T : comparison> = Map<Node, 'T AnalysisMapping>

// The pointed semi-lattice
type AnalysisDomain<'T when 'T : comparison> = 
  {
    relation: 'T AnalysisMapping -> 'T AnalysisMapping -> bool
    join: 'T AnalysisMapping -> 'T AnalysisMapping -> 'T AnalysisMapping
    bottom: 'T AnalysisMapping
  }

type AnalysisSpecification<'T when 'T : comparison> = 
  {
    domain: 'T AnalysisDomain
    mapping: Edge -> 'T AnalysisAssignment -> 'T AnalysisMapping
    initial: 'T AnalysisMapping
  }


let analyseMonotone (spec: 'T AnalysisSpecification) (pg: ProgramGraph) (worklist: Node IWorklist) : 'T AnalysisAssignment =
  let mutable step = 0
  let edgesWithStart (edges: Edge List) (qs: Node): Edge List =
    List.filter (fun (q, _, _) -> q = qs) edges

  let (qs, qe, edges) = pg;
  let mutable resultSet = Map.empty
  let mutable worklist = worklist.Empty

  for node in [qs .. qe] do
    resultSet <- resultSet.Add(node, spec.domain.bottom)
    worklist <- worklist.Insert(node)

  resultSet <- resultSet.Add(qs, spec.initial)

  while not worklist.IsEmpty do
    step <- step + 1
    let (q, worklist') = worklist.Extract
    worklist <- worklist'
    for e in (edgesWithStart edges q) do
      let newVal = spec.mapping e resultSet
      let (_, _, qe') = e
      let oldVal = resultSet.Item qe'
      if (not (spec.domain.relation newVal oldVal)) then
        resultSet <- resultSet.Add(qe', spec.domain.join oldVal newVal)
        worklist <- worklist.Insert(qe')
  printf "steps: %d\n" step
  resultSet
