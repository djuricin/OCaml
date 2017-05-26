let brojPoz=function
    |[]->0
    |lista->List.length(List.filter (fun a->a>0) lista)
;;


let rec prost1 n k=
    if n==k then true
    else if (n mod k)==0 then false
    else prost1 n (k+1);;

let prost n=prost1 n 2;;

let rec eliminisiProste=function
    |[]->[]
    | x::xs->x:: (eliminisiProste (List.filter (fun x->not (prost x)) xs))            
;;

let sab=function
    |[]->[]
    |lista->(List.map (fun x->x*(-1)) (List.filter (fun a->a<0) lista)) @ (List.filter(fun b->b>0) lista)
;;

let saberiAbs lista=List.fold_left (+) 0 (sab lista)
;;


let brojPojavljivanja e l = 
  let rec index_rec i = function
    | [] -> i
    | hd::tl -> if hd = e then index_rec (i+1) tl  else index_rec i tl
  in
  index_rec 0 l
;;

let saberi lista=List.fold_left (+) 0 lista
;;

let prosek=function
    |[]->0.0
    |lista-> (float_of_int (saberi lista))/.(float_of_int (List.length lista))
;;

let proizvod lista =List.fold_left ( * ) 1 lista;;

let rec nadoveziProizvod lista=
    match lista with
    |[]->[]
    |lista-> [(List.hd lista) @ (List.map (fun x-> proizvod x) [(List.hd lista)])] @ nadoveziProizvod (List.tl lista)
;;
  


