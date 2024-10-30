type orval =
  | Bool of bool
  | Closure of (orval -> orval);;
