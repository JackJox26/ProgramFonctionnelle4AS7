(* Questin 1.1 *)
let f a b = a + b
(* facon numero 1 *)
let ajout_deux g = if g = 0 then f 2 0
                  else f (2 + 1) (g - 1) 


(* facon numero 2 *)
let ajout_deux2 g = if g = 0 then f 2 0
                  else f (2 + 1 + g) (- 1)


(* Question 1.2 *)
let rec somme_derniers l1 l2 = 
  match l1 with
  |[] -> ( match l2 with
    |[] -> 0
    |x2 :: [] -> x2
    |hd2 :: tl2 -> somme_derniers [] tl2)
  |x1::[] -> ( match l2 with
    |[] -> x1
    |x2:: [] -> x1+ x2
    |hd2 :: tl2 -> somme_derniers [x1] tl2)
  |hd1:: tl1 -> somme_derniers tl1 l2

(* premier erreur "@" au lieu de "::" l23 *)
(* deuxieme erreur le "rec" de la fontion l14 *)
(* c'est la paranthèse manquantes pour l16-19 "( match l2 with ....  somme_derniers [] tl2)"" *)
(* c'est la paranthèse manquantes pour l20-23 "( match l2 with ....  somme_derniers [x1] tl2)"" *)


(* Quesiton 1.3 *)
let x a b = a + b
let f y z = 
  let v = y + 5 in
  if z>y then (Printf.printf "%d" z ; z)
  else x v 0

(* il manquait le "in" l35 *)
(* on voulait un ajout de int pas float donc pas de "+." mais  "+" ligne 33 *)
(* j'ai mis les paranthese pour le "(Printf.printf "%d" z ; z)" ligne 36 *)


(* Question 1.4 *)
let y = 5.3 ;;
(* y lié a y l45 *)
let u z = z -. y ;;
(* u lié u l57 ; z lié z l47 ; y lié y l45  *)
let y =
  (* y lié y l49 *)
  let y = y +. 2.5 
  (* y lié y l51 = y lié y l45 *)
  in let z = u y
  (* z lié z l47; u lié u l57 ; y lié y l51 *)
    in y *. z
    (* y lié y l51 ; z lié z l53 *)
      in let u = 2.0 
      (* u lié u l57 *)
        in y +. y +. u ;;
        (* y lié y l51 ; u lié u l57 *)
