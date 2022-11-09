open Printf
(* Printf.printf("Hello World! testons si ca fonctionne\n") *)
(* //lancer rlwrap ocaml pour voir les types dans une fonction *)
(* lancer #use "td1.ml";; *)
let rec mystere x =
  if x = 0 then [x] else x :: mystere(x-1)
let x = 1+2;;
print_int x;;
Printf.printf("\n");;

(* Exercice 1 *)
(* 1) int -> int*)
let f1 = fun x -> x + 1;; 
(* 2) float -> float -> float *)
let f2 = fun x y -> x *. y;;
(* 3) bool ->(bool*bool)-> (bool) *)
let f3 = fun x (y,z) -> if x then y else not z;;
(* 4) a -> a) *)
let f4 = fun x -> x;;
(* 5) a -> a -> c -> bool * c *)
let f5 = fun x y z -> (x <= y,z);;

(*Exercice 2
    ca renvoie une liste avec un ordre décroissant avec x la plus grande valeur  *)
let rec mystere x =
  if x = 0 then [x] else x :: mystere (x-1)
let a = mystere (3)
(* Printf.printf(a);; *)
let () = List.iter (printf "%d," ) a;;
Printf.printf("\n");;
type t = A of t | B of int * t| C
(* ca renvoie la somme de la fonction existant *)
let rec somme f n = 
  if n<= 0 then 0 else f n + somme f (n-1)
let g = somme (fun x -> x)
let () = Printf.printf "somme de 0 à 10 :";;
let () =  Printf.printf "%d\n" (g 10);;


(* Exercice 3
   Elle n est pas exhaustif pour etre exhaustif il faut mettre un default qui | _ -> 10*)
let g v = match v with
| A (C) -> 0
| A (B (x,C)) -> 2
| A (_) -> 1
| B (0,_) -> 3
| B (y, A (_) ) -> 4
| B (1,B(2,A(C))) -> 6
| C -> 5 
| _ -> 10 ;;


(* Exercie 4 *)
let rec h x y =
    if x = y then 1
    else
    if x < y then x + y + h (x - 1) (y - 2)
    else x * h (x - 1) y;;

let h1 x y =
  let rec h_terminal x y acc =
    if x = y then acc
    else
      if x < y then h_terminal (x-1) (y-2) (acc+x+y)
      else h_terminal (x-1) (y) (acc*x)
    in h_terminal x y 1


(* Exercice 5 *)
let f1bis =
  let rec f1_aux x acc = 
    if acc > 4 then acc
    else f1_aux x (acc+2)
  in f1_aux 0 0;;

let f2bis = 
  let rec f2_aux x y = 
    if x > 4 then (y-x)
    else f2_aux (x+2) (y-1)
  in f2_aux 0 10;;

let f3bis = 
  let rec f3_aux x y z =
    if x > y then z
    else f3_aux (x+1) (y-x) (z)
  in f3_aux 0 5 100;;


(* exercice 6 *)
let f6 =
  let rec f6_aux i acc = 
    if i > 3 then acc
    else f6_aux (i+1) (acc+2)
  in f6_aux 0 0;;
  

(* exercice 7 *)
let max x y =
  if x>y then x
  else y;;

let rec max_liste l =
  match l with 
    | [] -> assert false
    | [x] -> x
    | x::s -> max x (max_liste s)
(* in max_liste [2;-1;7;4;0;2];; *)

(* exercice 8 *)

let test x = x*x;;


let rec map f l1 =
  match l1 with
    |[] -> []
    |x::s -> (f x)::(map f s)
(* in map test [1;2;3;4];; *)


let rec propercuts1 la =
  match la with
  | [] -> [[],[]]
  | x::s -> (let r = propercuts1 s in ([],la):: List.map (fun(l1,l2) -> x::l1,l2) r)
(* in propercuts1 [1;2;3;4];; *)

(* exercice 9 *)
let calculSommeListe l=
  let rec cal_aux l acc =
    match l with
    |[]->acc
    | x::s -> cal_aux (s) (acc+x)
  in cal_aux l 0;;
(* in calculSommeListe [1;2;3;4];; *)


(* calculSommeListe *)
(* max_liste *)
let max_kadane l =
  let rec kadane_aux l m =
    match l,m with
    | [], _ -> max_liste m
    | x::r, y::_ ->
      let z = max x (x+y) in 
      kadane_aux r (z::m)
    |_ -> assert false
  in kadane_aux l [0]
(* in max_kadane [-2;1;-3;4;-1;2;1;-5;4];; *)

(* travaille fini pour l exo 9 *)


(* let f1 a =
  if a=2 then begin
    print_string "youpi 1";
    print_string "youpi 2";
    print_string "\n"
  end *)


(* let compteur i =
  let etat = ref i in
  fun () -> etat := !etat + 1; !etat ;; *)
(* type student = {number : int; mutable age :int}
let birthday e = e.age<- e.age+1
let e = {number = 12134; age =21}
let () = birthday e; print_int e.age *)


(* type 'a ref = { mutable content : 'a }
let ref = fun v -> { content = v }
let (:=) = fun r v -> r.contents <- v
let (!) = fun r -> r.contents *)

