
(* Run this file by starting the metaocaml command, then running #use "replicate.ml" ;; *)
(* Tested in 5.3.0+BER *)


let rec cons_n (n : int) (v : 'a) (l : 'a list) : 'a list =
  if n <= 0 then l
  else v :: cons_n (n - 1) v l;;

let rec replicate (n : int) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | a :: d -> cons_n n a (replicate n d);;

replicate 2 ["a"; "b"; "c"];;


(* Version using peano numerals, to more closely match our staged mK code *)

type peano =
  | Z
  | S of peano ;;

let rec cons_n (n : peano) (v : 'a) (l : 'a list) : 'a list =
  match n with
  | Z -> l
  | S p -> v :: cons_n p v l ;;

let rec replicate (n : peano) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | a :: d -> cons_n n a (replicate n d) ;;

replicate (S (S Z)) ["a"; "b"; "c"] ;;


(* Curried definition of replicate, as preparation for staging. *)

let rec replicate_c (n : peano) : 'a list -> 'a list =
  let rec replicate_n (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | a :: d -> cons_n n a (replicate_n d)
  in
  replicate_n ;;

replicate_c (S (S Z)) ["a"; "b"; "c"] ;;

(* Staged *)

let rec cons_n_staged (n : peano) (v : 'a code) (l : 'a list code) : 'a list code =
  match n with
  | Z -> l
  | S p -> .<.~v :: .~(cons_n_staged p v l)>. ;;

let rec replicate_staged (n : peano) : ('a list -> 'a list) code =
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


(* Version where the number is partially static, and we fall back to the runtime cons_n *)

type peanoPS =
  | Z
  | S of peanoPS
  | C of peano code ;;

let rec cons_n_ps (n : peanoPS) (v : 'a code) (l : 'a list code) : 'a list code =
  match n with
  | Z -> l
  | S p -> .<.~v :: .~(cons_n_ps p v l)>.
  | C nC -> .< cons_n .~nC .~v .~l >.;;

let rec replicate_ps (n : peanoPS) : ('a list -> 'a list) code =
  .<let rec replicate_n l =
     match l with
     | [] -> []
     | a :: d -> .~(cons_n_ps n .<a>. .<(replicate_n d)>.)
   in
   replicate_n>. ;;

let replicate_2plus = Runcode.run .<fun n l -> .~(replicate_ps (S (S (C .<n>.)))) l>. ;;
replicate_2plus (S Z) ["a"; "b"; "c"] ;;
