(* Exercice 2 *)


(* Question2.1 *)
let f1 (x,y) z =
  let g a b = a<b in
  if z then g x else g y;;

(* le type de z est bool car dans la ligne 7 on a if (z) et donc il peut que etre un bool *)
(* le type de x et y doit etre des int,float... qui peut etre considere comme de type 'a car dans la ligne 6 on a une comparaison entre a et b a<b 
et donc on peut que comparer des a qui peut etre des int,float...  *)


(* Question2.2 *)
let list_sum p = List.fold_left ( fun x y -> if p y then x + 1 else x ) 0;;
(* list_sum ajoute 1 si la fonction p return vrai pour chaque element de la liste et 
donc list_sum calcule le nombre d'element qui retourne vrai pour la fonction p *)
(* le type de list_sum est int *)

let list_or = List.fold_left ( fun x y -> x || y ) false ;;
(* list_or renvoie la comparaison or de tous les éléments de la liste des booléens fournie
en partant du premier élément qui est false *)
(* le type de list_or est boolean *)

let p1 a =
  if a = 0 then true
  else false 

let a = [];;
(* list_sum a ;; *)
(* pour "list_sum a" elle n'est pas bien typé car elle a besoin d'un paramètre qui manque qui est la fonction p dont "list_sum p" prends en paramètre
le type a est un *)
list_or a ;;
(* pour list_or elle est bien typé car comme elle reçoit une liste vide en paramètre elle peut quand même faire une comparaison pour une liste vide 
le type a est un bool list*)

let a = ref [];;
(* list_sum !a ;; *)
(* pour "list_sum !a" elle n'est pas bien typé car elle a besoin d'un paramètre qui manque qui est la fonction p dont "list_sum p" prends en paramètre
le type a est un *)

list_or !a ;;
(* pour list_or elle est bien typé car comme elle reçoit une liste vide en paramètre elle peut quand même faire une comparaison pour une liste vide 
le type a est un bool list*)


(* Question 2.3 *)
type 'a arbre_binaire = 
| Feuille
| Noeud of 'a * 'a arbre_binaire * 'a arbre_binaire;;

(* declaration d'arbre par défaut *)
let arbre1 = Noeud(1,Feuille,Feuille);;
let arbre2 = Noeud(0,Noeud(0,Noeud(0,Feuille,Feuille),Feuille),Noeud(0,Feuille,Feuille))

(* declaration de fonction par défaut *)
let f a = 
  if a = 0 then true
  else false

(* Écrire une fonction map_arbre qui prend en entrée un arbre binaire représenté par ce type et une fonction f, et qui renvoie l’arbre en transformant par f *)
let rec map_arbre (f:'a -> 'b) (arb : 'a arbre_binaire):'b arbre_binaire  =
  match arb with
  |Feuille -> Feuille
  |Noeud (x, enfant1, enfant2) -> Noeud(f x, map_arbre f enfant1, map_arbre f enfant2)
(* le type de cette fonction est de type "'b arbre_binaire" qui est renvoyé par la fonction f*)

(* Écrire une fonction forall_arbre qui prend en entrée un arbre binaire et un prédicat (fonction de type ’a -> bool), et renvoie true ssi toutes les étiquettes vérifient *)
let rec forall_arbre (f:'a -> bool) (arb : 'a arbre_binaire): bool =
  match arb with
  |Feuille -> true
  |Noeud (x, enfant1,enfant2) -> if (f x=false) then false 
                                else (forall_arbre f enfant1 && forall_arbre f enfant2)
            
(* le type de cette fonction est de type "bool" qui est renvoyé par la fonction f *)
