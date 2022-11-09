(* type p = {
ligne : int;
colonne : int;
} *)
type marque = {c:bool; p: int * int}
type grille = marque list


(* Question1 *)
let grille1 = [{c=true ; p=(0,0)};{c=false ; p = (0,2)};
{c = true ; p = (1,0)}; {c = false ; p = (1,1)};
{c = true ; p = (2,0)};{c = false; p = (2,2)}]

let grille2 = [{c=true ; p=(0,1)};{c=false ; p = (0,2)};
{c = true ; p = (1,0)}; {c = false ; p = (1,1)};
{c = false ; p = (2,0)};{c = true; p = (2,1)}]

let grille3 = [{c=false ; p=(0,1)};{c=true ; p = (0,2)};{c=true ; p = (0,3)};
{c = true ; p = (1,0)}; {c = false ; p = (1,1)};{c=false ; p = (1,2)};
{c = false ; p = (2,0)};{c = true; p = (2,1)};{c=false ; p = (2,2)}]


(* Question 2 *)
(* let f a = (fst a.p < 3 && fst a.p >=0) && (snd a.p < 3 && snd a.p >= 0)
let dans_les_bornes grille = List.for_all f grille *)

let dans_les_bornes grille = 
  let f a = (fst a.p <3 && fst a.p >=0) && (snd a.p <3 && snd a.p >=0) in
  List.for_all f grille

(* Question3 *)
let existe_symbole grille i j =
  let f1 a = a.p = (i,j) in
  List.exists f1 grille


(* Question 4 *)

let sans_doublons grille = 
  let rec sans_aux grille i j cpt =
    match grille with
    |[] -> true
    |x::s -> if (existe_symbole grille ) then sans_aux grille i j cpt=grille i j cpt1
          if (cpt > 1) then false
          else sans_aux s
    in sans_aux grille 0 0
  
(* Question 5 *)







(* test for all *)
(* let testForAll f l1 = List.for_all f l1
let f a = true *)

 
