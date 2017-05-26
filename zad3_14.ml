let sum1=function
    |[]->[]
    |lista->(List.map (fun x->x*x) (List.filter (fun a->a<0) lista));;

let sumaKvNeg lista=List.fold_left (+) 0 (sum1 lista);;


let malaSlova=function
    |[]->0
    |lista-> List.length(List.filter(fun x->x>='a' && x<='z') lista)
;;


let sume=function
    |[]->[]
    | lista->List.map (fun x->List.fold_left (+) 0 x) lista
;;


let spoji=function
    | []->[]
    |lista->List.concat lista
;;

(*let rec parOdListi(l1,l2)=function
    |([],_)->[]
    |(_,[])->[]
    |((x::xs),(y::ys))->((x,y),(parOdListi xs ys))
;;*)

(*let rec ucesljaj (l1,l2)=
    match (l1,l2) with
    |([],l2)->l2
    |(l1,[])->l1
    |(l1,l2)->(List.hd l1) @ (List.hd l2) @ (ucesljaj ((List.tl l1),(List.tl l2)))
;;*)
