type grid = int list list
exception NotConformTable
exception ListVide
(* Question 3.1 *)
let test = [[0;1];[2;3]]
let g_example : grid =[[-2; 0;1;4]; [7; 2;-3;-4];[6; -1;3;5]]
let exception1 = [[]]


(* Question 3.2 *)
let height1 (g) =
  if g = [] then assert false
  else
    let rec height_aux g acc = 
      match g with
        |[] -> acc
        |x::s -> height_aux s (acc+1)
    in height_aux g 0

let height (g:grid) = 
  if (g =[]) then raise ListVide
  else List.fold_left (fun x y -> x+1) 0 (List.hd g) 

(* Question 3.3 *)
let rec first_element list = 
  match list with 
     | [] -> failwith "List is empty"
     | first_el::rest_of_list -> first_el

let wf_grid_exn g =
  let f a = if height1 (a) = 0 then assert false
  else height1 (a) = height1(first_element (g))
  in List.for_all f g


(* Question 3.4 *)
(* Le nombre maximale qu on peut obtenir en faisant un chemin valide est 4 + 7 + 6 = 17 *)


(* Question 3.5 *)
let videL = []
let l1 = [2;3;4;5;6;7;8;9;10]

let rotate_up l =
  let rec up_aux l l2 i res0 lenl=
    match l with
    |[] -> l2
    |x::s -> if (i = 0) then up_aux s l2 (i+1) x lenl
             else(
              if (i = (lenl) -1) then up_aux s (res0::x::l2) (i+1) res0 lenl
              else up_aux s (x::l2) (i+1) res0 lenl
             ) 
  in List.rev (up_aux l [] 0 0  (height1 (l)))


(* Question 3.6  *)
let last_element l =
  let rec last_aux l =
    match l with
    |[] -> failwith "List is empty"
    |x2::[] -> x2
    |x::s -> last_aux s
  in last_aux l

let rotate_down l =
  let rec down_aux l l2 i lenl=
  match l with
  |[] -> l2
  |x::s -> if (i = lenl -1) then down_aux s l2 (i+1) lenl 
           else down_aux s (x::l2) (i+1) lenl 
  in List.rev (down_aux l [last_element l] 0 (height1(l)))


(* Question 3.7 *)
let rec first_element list = 
    match list with 
       | [] -> failwith "List is empty"
       | first_el::rest_of_list -> first_el
    
let best_option l1 =
  let haut = rotate_up l1 in let bas = rotate_down l1 in
  let rec aux l1 haut bas = match l1 with
  |[] -> []
  |h::t -> (max h( max (first_element haut) (first_element bas)))::aux t (List.tl haut) (List.tl bas)
in aux l1 haut bas ;;

(* Question 3.8 *)

let sums (g:grid) = 
  let rec sum l1 l2 = 
    match l1,l2 with
    | [],[] -> []
    | h1::t1,h2::t2 -> (h1+h2)::sum t1 t2
    | _-> raise NotConformTable
  in 
  let zero n= List.init n (fun _ -> 0) in
  let rec aux g acc = match g with
    | [] -> acc
    | h::t -> aux t (sum h (best_option acc))
  in aux g (zero (height g));;


(* Question 3.9 *)
let max_list l = 
  let rec max_aux l max=
    match l with
    |[] -> max
    |x::s -> if (x > max) then max_aux s x
             else max_aux s max
  in max_aux l (first_element l) 

(* Question 3.10 *)
let solve (g:grid) = 
  let rec solve_aux l acc =
    match l with
    |[] -> acc
    |x::s -> solve_aux s (acc+x)
  in solve_aux (sums g) 0

(* let sums (g:grid) =
  let rec sums_aux l1 l2 =
    match l1,l2 with
    |[],[] -> []
    |h1::t1, h2::t2 -> (h1+h2)::sums_aux t1 t2
    |_-> raise (Failure "grid avec porbleme")
  in
  let zero n = List.init n (fun _ -> 0) in
  let rec aux g acc = 
    match g with
      |[] -> acc
      |h::t -> aux t (sums_aux h (best_option acc))
  in aux g (zero (height1 g));; *)


(* let sums (g:grid) =
  let rec sum l1 l2 = match l1,l2 with
  |[],[] -> []
  |h1::t1, h2::t2 -> (h1+h2)::sum t1 t2
  | _ -> raise (Failure "fonctionne pas")
  in
  let zero n = List.init n (fun _ -> 0) in
  let rec aux g acc = match g with
  |[] -> acc
  |h::t -> aux t (sum h (best_option acc))
in aux g (zero (height1 g));; *)





(* let rotate_down l =
  let rec down_aux l l2 i res0 lenl =
  match l with
  |[] -> l2
  |x::s -> if (i = (lenl) -1) then down_aux s l2 (i+1) x lenl
            else(
              down_aux s (x::l2) (i+1) res0 lenl
            )
  in List.rev (down_aux l [last_element l] 0 0 (height1 (l))) *)
(* let f a = (fst a.p <3 && fst a.p >=0) && (snd a.p <3 && snd a.p >=0) in     *)
(* let wf_grid_exn g =
  let rec wf_grid_exn g acc temp=
    match g with
      |[] -> true
      |x::s -> List.for_all  *)