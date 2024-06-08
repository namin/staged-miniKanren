(* Run this file by starting the metaocaml command, then running #use "replicate.ml" ;; *)

let rec cons_n (n : int) (v : 'a) (l : 'a list) =
  if n <= 0 then l
  else v :: cons_n (n - 1) v l;;

let rec replicate (n : int) (l : 'a list) =
  match l with
  | [] -> []
  | a :: d -> cons_n n a (replicate n d);;

replicate 2 ["a"; "b"; "c"];;


(* Version using peano numerals, to more closely match our staged mK code *)

type peano =
  | Z
  | S of peano ;;

let rec cons_n (n : peano) (v : 'a) (l : 'a list) =
  match n with
  | Z -> l
  | S p -> v :: cons_n p v l ;;

let rec replicate (n : peano) (l : 'a list) =
  match l with
  | [] -> []
  | a :: d -> cons_n n a (replicate n d) ;;

replicate (S (S Z)) ["a"; "b"; "c"] ;;


(* Curried definition of replicate, as preparation for staging. *)

let rec replicate_c (n : peano) =
  let rec replicate_n (l : 'a list) =
    match l with
    | [] -> []
    | a :: d -> cons_n n a (replicate_n d)
  in
  replicate_n ;;

replicate_c (S (S Z)) ["a"; "b"; "c"] ;;

(* Staged *)

let rec cons_n_staged (n : peano) (v : 'a code) (l : 'a list code) =
  match n with
  | Z -> l
  | S p -> .<.~v :: .~(cons_n_staged p v l)>. ;;

let rec replicate_staged (n : peano) =
  .<let rec replicate_n l =
     match l with
     | [] -> []
     | a :: d -> .~(cons_n_staged n .<a>. .<(replicate_n d)>.)
   in
   replicate_n>. ;;

let cons2 = Runcode.run .< fun v l -> .~(cons_n_staged (S (S Z)) .<v>. .<l>.) >.;;
cons2 "a" ["b"; "b"; "c"; "c"];;

let replicate_2 = Runcode.run (replicate_staged (S (S Z))) ;;
replicate_2 ["a"; "b"; "c"] ;;
