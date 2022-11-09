(* exo Arithmétique intervalles *)


type interval = {
inf : int;
sup : int;
}

let rec make_interval a b =
  if b > a then {inf = a; sup = b} 
  else {inf = b ; sup = a}

let fOperation op enrA enrB =
  let minA a b c d = min (min a b) (min c d) in
  let maxB e f g h = max (max e f) (max g h) in 
  {
  inf = minA (op enrA.inf enrB.inf) (op enrA.inf enrB.sup) (op enrA.sup enrB.inf) (op enrA.sup enrB.sup) ;
  sup = maxB (op enrA.inf enrB.inf) (op enrA.inf enrB.sup) (op enrA.sup enrB.inf) (op enrA.sup enrB.sup)
  }
let fAdd = fOperation (+)
let fSub = fOperation (-)
let fmul = fOperation ( * )
(* taper #use "td3.ml" *)
(* taper fmul (make_interval 2 4) (make_interval 3 5);; *)

(* exo Echauffement sur les listes *)

let rec rev_append l1 l2 =
  match l1 with
  | [] -> l2
  | x :: s -> rev_append s (x :: l2)

let rev l = rev_append l []

type t = B | N | R
let permute l = 
  let rec per_aux l l0 =
    match l with
    |[] -> l0
    |x::s -> 
      if x = B then per_aux (s) (N::l0)
      else if x = N then per_aux (s) (R::l0)
      else per_aux (s) (B::l0) 
  in rev (per_aux l [])


let compteB l =
  let rec comp_B_aux l acc=
    match l with 
    |[] -> acc
    |x::s -> if x = B then comp_B_aux s (acc+1)
              else comp_B_aux s (acc)
    in comp_B_aux l 0


let rec liste_max l = 
  match l with
    |[x] -> x
    |t::q -> max t (liste_max q)
    |[] -> failwith "Liste vide!";; 


let plus_grande_sequence_de_B l =
  let rec plus_grand_nb_B l l0 acc =
     match l with
     |[] -> if acc = 0 then liste_max (l0)
            else plus_grand_nb_B l (acc::l0) 0
     |x::s -> if x = B then plus_grand_nb_B s l0 (acc+1)
              else plus_grand_nb_B s (acc::l0) 0
  in plus_grand_nb_B l [] 0

(* lus_grande_sequence_de_B [B;B;B;B;R;R;B;B;B;B;B];; *)

(* let remplace l =
  let rec rempl_aux l *)