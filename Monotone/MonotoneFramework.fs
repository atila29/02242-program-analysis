module MonotoneFramework

open ProgramGraph

type AnalysisAssignment<'T when 'T : comparison> = Map<Node, 'T Set>

// The pointed semi-lattice
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

let analyse (spec: 'T AnalysisSpecification) (pg: ProgramGraph) : 'T AnalysisAssignment =
  let needsUpdating (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : bool =
    List.exists (fun edge -> 
      let newVal = spec.mapping edge resultSet
      let (_, _, qe) = edge;
      not (spec.domain.relation newVal (resultSet.Item qe))
    ) edges

  let updateEdges (spec: 'T AnalysisSpecification) (edges: Edge List) (resultSet: 'T AnalysisAssignment) : 'T AnalysisAssignment =
    List.foldBack (fun (qs, action, qe) acc -> 
      let newValue = spec.mapping (qs, action, qe) resultSet
      let oldValue = resultSet.Item qe
      acc.Add(qe, spec.domain.join oldValue newValue)
    ) edges resultSet

  let (qs, qe, edges) = pg;
  let mutable resultSet = ([qs + 1 .. qe] |> List.map (fun i -> i, spec.domain.bottom) |> Map.ofList)
  resultSet <- resultSet.Add(qs, spec.initial)

  while needsUpdating spec edges resultSet do
    resultSet <- updateEdges spec edges resultSet

  resultSet
