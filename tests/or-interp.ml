type orexp1 =
  | Lit of bool
  | Or of orexp1 * orexp1
  | Sym of string

type env1 = (string * bool) list ;;

let rec eval_or1 (e : orexp1) (env : env1) =
  match e with
  | Lit b -> b
  | Or (e1, e2) -> eval_or1 e1 env || eval_or1 e2 env
  | Sym s -> List.assoc s env ;;

eval_or1 (Or ((Sym "a"), (Lit true))) [("a", true)] ;;


type orexp2 =
  | Lit of bool
  | Or of orexp2 * orexp2
  | Sym of string
  | Lam of string * orexp2
  | App of orexp2 * orexp2 ;;

type orval2 =
  | Bool of bool
  | Closure of (orval2 -> orval2) ;;

type env2 = (string * orval2) list ;;

let rec eval_or2 (e : orexp2) (env : env2) =
  match e with
  | Lit b -> Bool b
  | Or (e1, e2) -> (match eval_or2 e1 env with
                    | Bool v1 -> (match eval_or2 e2 env with
                                  | Bool v2 -> Bool (v1 || v2)))
  | Lam (s, e) -> Closure (fun v -> eval_or2 e ((s, v) :: env))
  | App (e1, e2) -> (match eval_or2 e1 env with
                     | Closure f -> f (eval_or2 e2 env))
  | Sym s -> List.assoc s env ;;

eval_or2 (App ((Lam ("x", (Or ((Sym "x"), (Sym "x"))))), (Sym "a"))) [("a", Bool true)];;


type env1_staged = (string * bool code) list ;;

let rec eval_or1_staged (e : orexp1) (env : env1_staged) =
  match e with
  | Lit b -> .< b >.
  | Or (e1, e2) -> .< .~(eval_or1_staged e1 env) || .~(eval_or1_staged e2 env) >.
  | Sym s -> List.assoc s env ;;


let f = Runcode.run .< fun a -> .~(eval_or1_staged (Or ((Sym "a"), (Lit true))) [("a", .< a >.)]) >. ;;
f true;;
f false;;

(** The ordecls.ml file has to be separately compiled with this command first:
   metaocamlc -c ordecls.ml   *)

#use "ordecls.ml";;
open Ordecls;;

type orexp =
  | Lit of bool
  | Or of orexp * orexp
  | Sym of string
  | Lam of string * orexp
  | App of orexp * orexp ;;

type env = (string * orval code) list ;;

let rec eval_or (e : orexp) (env : env) : orval code =
  match e with
  | Lit b -> .< Bool b >.
  | Or (e1, e2) ->
    .< (match .~(eval_or e1 env) with
        | Bool false -> .~(eval_or e2 env)
        | v1 -> v1) >.
  | Sym s -> List.assoc s env
  | Lam (s, e) -> 
    .< Closure
       (fun v -> .~(eval_or e ((s, .<v>.) :: env))) >.
  | App (e1, e2) ->
    .< (match .~(eval_or e1 env) with
        | Closure f -> f .~(eval_or e2 env)) >. ;;

let f = Runcode.run .< fun a -> .~(eval_or (App ((Lam ("x", (Or ((Sym "x"), (Sym "x"))))), (Sym "a"))) [("a", .< Bool a >.)]) >.;;

f true;;
f false;;