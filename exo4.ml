(* Question 4.1 *)
let rec fibo n =
  if n <0 then raise (Failure "nombre fibo négatif")
  else 
    if n >1 then fibo (n-1) + fibo (n-2)
    else n


(* Question 4.2 *)
let count = ref 0;;
let rec fibocount n =
  count := !count + 1;
  if n <0 then raise (Failure "nombre fibo négatif")
  else 
    if n >1 then fibocount (n-1) + fibocount (n-2)
    else n

(* la loi de croissance du nombre count est multiplié par 2 ou 3 a chaque appel de la fonction et donc la loi de croissance est exponentille de fonction 2^n ou 3^n
n= 6: count = 25
n= 7: count =41
n= 8: count = 67
n= 9: count = 109
n =10: count =177
n= 11: count =355 
n= 12: count =711 *)


(* Question 4.3 *)
let f1 =
  let rec f_aux l i=
    if (i=1024) then l
    else f_aux ((-1)::l) (i+1)
  in f_aux ([]) (0)

let f = Array.init 1024 (fun i -> -1);;

let rec memofibocount n =
  count := !count +1;
  if n < 0 then raise (Failure "nombre fibo négatif")
  else if n <= 1 then n
  else fibocheck (n-1) + fibocheck (n-2) and fibocheck n =
  if f.(n) = -1 then
    begin
      f.(n) <- memofibocount n;
      f.(n)
    end
  else f.(n);;

(* Question 4.4 *)
(* la loi de croissance du nombre count est ajouté par 1 a chaque appel et donc la loi de croissance est linéaire de fonction n
n=6:count =7
n=7:count =8
n=8:count =9
n=9:count =10
n=10:count =11
n=11:count =12*)