type command = Up | Down | Left | Right | Seq of command list
type coordonnee = int * int
let coorTortue = (0,0);;
(* let a =  ((fst coorTortue)+1, (snd coorTortue)+1);; *)
let c1 = Seq[Up;Left;Left;Left;Left];;
let c_aux1 =(Seq[Up;Down],2);;

(* Querstion 5.1 *)
let evalpos (c:command) =
  let rec eval_aux c coorTor=
    if (fst coorTor > -100 && fst coorTor < 100 && snd coorTor > -100 && snd coorTor <100) then
      match c with
        |Up -> (0,1)
        |Down -> (0,-1)
        |Left -> (-1,0)
        |Right -> (1,0)
        |Seq(l) -> (match l with
          |[] -> coorTor
          |x::s -> match x with 
            |Up -> eval_aux (Seq (s)) (fst coorTor,snd coorTor+1)
            |Down -> eval_aux (Seq (s)) (fst coorTor,snd coorTor-1)
            |Left -> eval_aux (Seq(s)) (fst coorTor-1,snd coorTor)
            |Right -> eval_aux (Seq (s)) (fst coorTor+1,snd coorTor)
            |Seq(m) -> eval_aux (Seq(s)) (fst coorTor,snd coorTor))
    else raise (Failure "erreur de sortie")
  in eval_aux c (0,0)

(* Question5.2 *)
type command = Up | Down | Left | Right | Seq of command list | Repeat of int * command

let rec evalpos2 c = 
  let rec aux c (x1,y1) = 
    match c with
    | Seq [] -> (x1,y1)
    | Down -> (x1,y1-1)
    | Up -> (x1,y1+1)
    | Left -> (x1-1,y1)
    | Right -> (x1+1,y1)
    | Seq (h::t) -> aux (Seq t) (aux h (x1,y1))
    | Repeat (n, c) -> if n = 0 then (x1,y1) else aux (Repeat (n-1, c)) (aux c (x1,y1))
  
  in
  let (x1,y1) = aux c (0,0) in
  if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "La tortue n est pas dans le terrain")
  else (x1,y1);;

evalpos2(Repeat(10,Seq[Repeat(4,Up);Down]));;


(* Question 5.3 *)

type command = Up | Down | Left | Right | Seq of command list | Repeat of command
let rec evalposBoucle c = 
  let rec eval_aux c (x1,y1) = 
    begin
    match c with
    | Seq [] -> if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")else (x1,y1)
    | Down ->if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")else (x1,y1-1)
    | Up -> if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")else (x1,y1+1)
    | Right -> if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")else (x1+1,y1)
    | Left -> if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")else (x1-1,y1)
    | Seq (h::t) -> eval_aux (Seq t) (eval_aux h (x1,y1))
    | Repeat  c -> if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100  then (x1,y1) else eval_aux  c (eval_aux c (x1,y1))
    end;
    
  in
  let (x1,y1) = eval_aux c (0,0)
  in
  if x1 < -100 || x1 > 100 || y1 < -100 || y1 > 100 then raise (Failure "Tortue en dehors du terrain")
  else (x1,y1);;
;;

 let safety c =
  try
      let finale = evalposBoucle c in
      finale = (0,0)
  with Failure(s) -> false;;
  let c=Repeat (Seq[ Up;Down]);; 

  safety c;; 



let c1 = Repeat (Seq[ Down;Up;Down;Down]);; 
let c2 = Repeat (Seq[ Down;Up]);; 
safety c1;;
safety c2;;
(* let rec evalposBoucle c = 
  let rec aux c (x,y) = 
    begin
    match c with
    | Up -> if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")else (x,y+1)
    | Down ->if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")else (x,y-1)
    | Left -> if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")else (x-1,y)
    | Right -> if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")else (x+1,y)
    | Seq [] -> if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")else (x,y)
    | Seq (h::t) -> aux (Seq t) (aux h (x,y))
    | RepeatInfinie  c -> if x < -100 || x > 100 || y < -100 || y > 100  then (x,y) else aux  c (aux c (x,y))
    
    end;
    
  in
  let (x,y) = aux c (0,0)
  in
  if x < -100 || x > 100 || y < -100 || y > 100 then raise (Failure "Tortue sortie du terrain")
  else (x,y);;
;;

 let safety c =
  try
      let finale = evalposBoucle c in
      finale = (0,0)
  with Failure(s) -> false;;
  let c=RepeatInfinie (Seq[ Up;Down]);; 

  safety c;;  *)



 (* let evalpos (c:command) =
  let rec eval_aux c x1 y1 =
    if (x1 > -100 && x1 < 100 && y1 > -100 && y1 <100) then
      match c with
        |Up -> (0,1)
        |Down -> (0,-1)
        |Left -> (-1,0)
        |Right -> (1,0)
        |Seq(l) -> (match l with
          |[] -> (x1,y1)
          |e::s -> match e with 
            |Up -> eval_aux (Seq (s)) x1 (y1+1)
            |Down -> eval_aux (Seq (s)) x1 (y1-1)
            |Left -> eval_aux (Seq(s)) (x1-1) y1
            |Right -> eval_aux (Seq (s)) (x1+1) y1
            |Seq(m) -> eval_aux (Seq(s)) x1 y1)
    else raise (Failure "erreur de sortie")
  in eval_aux c 0 0  *)


  
(* let evalpos (c:command) =
  let rec eval_aux c coorTor=
    if (fst coorTor > -100 && fst coorTor < 100 && snd coorTor > -100 && snd coorTor <100) then
      match c with
        |Up -> (0,1)
        |Down -> (0,-1)
        |Left -> (-1,0)
        |Right -> (1,0)
        |Seq(l) -> (match l with
          |[] -> coorTor
          |x::s -> match x with 
            |Up -> eval_aux (Seq (s)) (fst coorTor,snd coorTor+1)
            |Down -> eval_aux (Seq (s)) (fst coorTor,snd coorTor-1)
            |Left -> eval_aux (Seq(s)) (fst coorTor-1,snd coorTor)
            |Right -> eval_aux (Seq (s)) (fst coorTor+1,snd coorTor)
            |Seq(m) -> eval_aux (Seq(s)) (fst coorTor,snd coorTor))
    else raise (Failure "erreur de sortie")
  in eval_aux c (0,0) *)


 (* let evalpos (c:command) =
  let rec eval_aux c x1 y1 =
    if (x1 > -100 && x1 < 100 && y1 > -100 && y1 <100) then
      match c with
        |Up -> (0,1)
        |Down -> (0,-1)
        |Left -> (-1,0)
        |Right -> (1,0)
        |Seq(l) -> (match l with
          |[] -> (x1,y1)
          |e::s -> match e with 
            |Up -> eval_aux (Seq (s)) x1 (y1+1)
            |Down -> eval_aux (Seq (s)) x1 (y1-1)
            |Left -> eval_aux (Seq(s)) (x1-1) y1
            |Right -> eval_aux (Seq (s)) (x1+1) y1
            |Seq(m) -> eval_aux (Seq(s)) x1 y1)
    else raise (Failure "erreur de sortie")
  in eval_aux c 0 0  *)

(* let evalpos (c:command list) =
  let rec eval_aux c x1 y1 =
  match c with
    |[] -> if (fst coorTor <= 100 && snd coorTor <=100 && fst coorTor >= -100 && snd coorTor >= -100) then coorTor
           else assert false 
    |x::s -> (match x with
      |Up -> eval_aux s (fst coorTor ,(snd coorTor)+1)
      |Down -> eval_aux s (fst coorTor ,(snd coorTor)-1)
      |Left -> eval_aux s ((fst coorTor)-1,snd coorTor)
      |Right -> eval_aux s ((fst coorTor)+1,snd coorTor)
      |Seq command -> eval_aux s coorTor)

  in eval_aux c coorTortue  *)

  (* type command2 = Up | Down | Left | Right | Seq of command list| int *command 
let evalpos2 (c:command2)=
  let rec eval_aux2 c coorTor :int*int =
    match (fst c) with
      |[] -> if (fst coorTor <= 100 && snd coorTor <=100 && fst coorTor >= -100 && snd coorTor >= -100) then coorTor
             else assert false 
      |x::s -> (match x with
        |Up -> eval_aux2 s (fst coorTor ,(snd coorTor)+1)
        |Down -> eval_aux2 s (fst coorTor ,(snd coorTor)-1)
        |Left -> eval_aux2 s ((fst coorTor)-1,snd coorTor)
        |Right -> eval_aux2 s ((fst coorTor)+1,snd coorTor)
        |Seq command -> eval_aux2 s coorTor)
        |(n,r) -> for ()
  
    in eval_aux2 c coorTortue  *)

