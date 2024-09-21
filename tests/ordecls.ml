type orval2 =
  | Bool of bool
  | Closure of (orval2 -> orval2);;