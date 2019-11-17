module MonotoneFramework

type Lattice<'T when 'T : comparison> = 
  {
    partialOrder: Set<'T> -> Set<'T> -> bool
    join: Set<'T> -> Set<'T> -> Set<'T>
    bottom: Set<'T>
  }

