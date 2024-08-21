type orexp =
  | Lit of bool
  | Or of orexp * orexp
  | Sym of string ;;

type env = (string * bool) list ;;

let rec eval_or (e : orexp) (env : env) =
  match e with
  | Lit b -> b
  | Or (e1, e2) -> eval_or e1 env || eval_or e2 env
  | Sym s -> List.assoc s env ;;

eval_or (Or ((Sym "a"), (Lit true))) [("a", true)] ;;


let rec eval_or (e : orexp) (env : env) =
  match e with
  | Lit b -> b
  | Or (e1, e2) -> eval_or e1 env || eval_or e2 env
  | Sym s -> List.assoc s env ;;

type env_staged = (string * bool code) list ;;

let rec eval_or_staged (e : orexp) (env : env_staged) =
  match e with
  | Lit b -> .< b >.
  | Or (e1, e2) -> .< .~(eval_or_staged e1 env) || .~(eval_or_staged e2 env) >.
  | Sym s -> List.assoc s env ;;


let f = Runcode.run .< fun a -> .~(eval_or_staged (Or ((Sym "a"), (Lit true))) [("a", .< a >.)]) >. ;;
f true;;
f false;;