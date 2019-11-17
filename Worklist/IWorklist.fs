module Worklist.Interface

type IWorklist<'T> = 
  abstract member Empty : IWorklist<'T>

  abstract member IsEmpty : bool

  abstract member Insert : 'T -> IWorklist<'T>

  abstract member Extract : 'T * IWorklist<'T>