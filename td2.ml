open Printf
(* //lancer rlwrap ocaml pour voir les types dans une fonction *)
(* lancer #use "td2.ml";; *)

(* I *)
(* exercice 1 *)
let f1 x g z = if x then g x else g z;;
let f2 f g x = if f x then g x;;
(* let f3 g x y = if g x then g y;; //faux il y a un probleme de typage *)
let rec f4 x y = f4 y ([]::x);;


(* II *)
(* exercice1 *)
let somme_carre n = 
  let rec somme_carre_aux n acc i =
    if n < i then acc
    else somme_carre_aux (n) (acc+i*i) (i+1)
  in somme_carre_aux n 0 0;;
(* in somme_carre 10;; *)


(* exercice 2 *)
(* normal *)
let rec somme l =
  match l with
  | [] -> 0
  | x::s -> x + somme s;;
(* in somme [2;3;5];; *)

(* recursive terminal *)
let somme1 l = 
  let rec somme_aux l acc =
    match l with
    | [] -> acc
    | x::s -> somme_aux s (acc+x)
  in somme_aux l 0;;
(* in somme1 [2;3;5];; *)


(* exercice 3 *)
(* normal0 *)
let rec fib n =
  if n <= 1 then n
  else fib (n-1) + fib (n-2);;
(* in fib 9;; *)

let f x =
  let rec f_cps x cont =
    if x <= 1 then cont x
    else f_cps (x-1) (fun r -> cont (f_cps (x-2) (fun a -> a + r)))
    in
    f_cps x (fun y -> y);;
  (* in f 9;; *)


(* III *)
(* exercice 4 *)

type parenthese = PO | PF
type mot = parenthese list

(* exrcice 5 *)
let ouvrante_ou_fermante cpt mot =
  match mot with
  | PO -> cpt+1
  | PF -> cpt-1
(* in ouvrante_ou_fermante 2 PO;; *)
let () = Printf.printf "%d\n" (ouvrante_ou_fermante 2 PO);;

(* exercice 6 *)
let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: s -> rev_append s (x :: l2)
let rev l = rev_append l [];;


let compte_parenthses mot = 
  let rec compte cpt mot liste =
    match mot with
    |[] -> liste
    |x::m -> compte (ouvrante_ou_fermante cpt x ) (m) ((ouvrante_ou_fermante cpt x)::liste)
  in rev(compte 0 mot []);;
(* in compte_parenthses [PO;PO;PF;PF;PF];; *)


(* exercie 7 *)
let postiif x =
  if x <0 then false
  else true;;


let rec tous_positif l = 
  match l with
  |[] -> true
  |x::s -> if x < 0 then false else tous_positif s;;
(* in positif_liste [1;5;6;7;4;219];; *)
(* exrcice 8 *)


let rec dernier_a_zero l =
  match l with
  |[]-> true
  |x::s -> if (s = []) && (x != 0) then false else dernier_a_zero s;;
(* in dernier_a_zero [1;2;3;4;5;6;0;0];; *)


(* exercice 9 *)
let est_un_mot_de_dyck l =
  let rec mot_dyck_aux s =
    if (tous_positif (s) && dernier_a_zero (s)) then true
    else false
    
  in mot_dyck_aux (compte_parenthses (l));;
(* in est_un_mot_de_dyck [PO;PO;PF;PF;PO;PF];; *)

(* exercice 10 *)
let max x y =
  if x>y then x
  else y;;

let sommet l =
  let rec sommet_aux l2 =
    match l2 with 
      | [] -> assert false
      | [x] -> x
      | x::s -> max x (sommet_aux s)
  in sommet_aux (compte_parenthses(l))
(* in sommet [PO;PO;PF;PO;PO;PO;PF;PF;PF;PF];; *)


(* IV *)


(* exercice 11 *)
let liste_n n =
  let rec liste_n_aux n acc =
    if n < 1 then acc
    else liste_n_aux (n-1) (n::acc)
  in liste_n_aux n [];;
(* in liste_n 31;; *)

(* exercice 12 *)


  