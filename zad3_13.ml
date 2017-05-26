let pozicije e l = 
  let rec index_rec i = function
    | [] -> []
    | hd::tl -> if hd = e then [i] @ index_rec (i+1) tl  else index_rec (i+1) tl
  in
  index_rec 0 l
  ;;
  
  
let rec quicksort = function
    | [] -> []
    | x::xs -> let smaller, larger = List.partition (fun y -> y < x) xs
               in quicksort smaller @ (x::quicksort larger)
;;


let rec brisiPonavljanja=function
    |[]->[]
    | x::xs->x:: (brisiPonavljanja (List.filter (fun a->a<>x) xs))            
;;

let rec podlistePonavljanja=function
    |[]->[]
    |x::xs->(x::(List.filter (fun a->a=x) xs)):: podlistePonavljanja(List.filter (fun b->b<>x) xs)
;;


let rec imaDuplikata l = match l with
    [] -> false
    | (h::t) ->
       let x = (List.filter (fun x -> x = h) t) in
         if (x == []) then
            imaDuplikata t
         else
       true;;
       
       
let rec uzastopniParovi l= match l with
    |[]->[]
    |x::xs-> List.combine x (List.hd xs)
;;


let broj ls = List.fold_right (fun x acc -> x + acc * 10) ls 0
;;


let broj1 ls = List.fold_right (fun x acc -> x + acc * 10) (List.rev ls) 0
;;
