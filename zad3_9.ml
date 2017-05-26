

let bezbedanRep1 lista=
    if lista==[] then []
    else List.tl lista;;
    

let bezbedanRep4 lista=
    match lista with
    []->[]
    | _-> List.tl lista;;
    
let bezbedanRep2=function
     []->[]
    | lista->  List.tl lista;;
    
    
let bezbedanRep3 lista=List.tl lista;;

(* savrseni zbirPar u jeziku Ocaml ne postoje generatori liste*)

let rec range1 i j = 
    if i>j 
    then [] 
    else i::(range1 (i+1) j);;
    
let range j=range1 1 j;;   
    
let sum lista= List.fold_left (+) 0 lista;;

let rec delioci1 n k=
    if k==n then [1]
    else if (n mod k)==0 then [k] @ (delioci1 n (k+1))
    else delioci1 n (k+1);;
    
let rec delioci n=
    if n==1 then [1]
    else delioci1 n 2;;
    

let sum1 n=sum (delioci n);;

let rec savrseni n=
    match n with
    |0->[]
    |_ -> if n=(sum1 n) then  n :: (savrseni (n-1)) else savrseni (n-1)
;;


let rec polovina xs =
    match xs with
  | x::y::tail ->
        let a,b = polovina tail in
        x::a, y::b
  | x::[] -> [x],[]
  | [] -> [],[];;   
  
  
let poslednji lista=List.nth lista ((List.length lista)-1);; 
  
let spoji lista=List.concat lista;;
  
let rec sufiksi =function
     []->[[]]
    | (x::xs)->(x::xs)::sufiksi xs;;
    

let rec obrni1 ls rls=
    match ls with
        | []-> rls
        | h::t ->obrni1 t rls@[h]
;;

let obrni ls=obrni1 ls [];;

(* -izbaci k lista;-izbaciSvaki k x lista u jeziku Ocaml nije moguce izbaciti element iz liste*)

let rec duplira=function
    []->[]
    |(x::xs)->[x;x]@duplira xs;;
    
    
let rec ubaci k n lista=
    if k==0 then n::lista
    else if lista==[] then [n]
    else (List.hd lista)::(ubaci (k-1) n (List.tl lista));;
    
    
    
let rec nadji index=function
    |[]->raise(Failure "empty list")
    |h::_ when index=0->h
    |_::t->nadji(index-1) t
;;
    
let izbaci i lista=List.filter (fun x-> x<>(nadji i lista)) lista
;;

