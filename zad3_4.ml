let rec proizvodPrvih n=
    if n==1 then 1
    else n*proizvodPrvih(n-1);;
    

let rec prost1 n k=
    if n==k then true
    else if (n mod k)==0 then false
    else prost1 n (k+1);;

let prost n=prost1 n 2;;
    
let rec nzd a b=
    if b==0 then a
    else nzd b (a mod b);;
    
    
    
let tipJednacine a b c=
    if a==0 then Printf.printf "degenerisana"
    else if (b*b-4*a*c)==0 then Printf.printf "jedno resenje"
    else if (b*b-4*a*c)>0 then Printf.printf "dva resenja"
    else Printf.printf "nema resenja";;
    
    
let rec izDekadne x osn=
    if x==0 then 0
    else izDekadne (x/osn) osn*10 + (x mod osn);;
    
let rec uDekadnu x osn=
    if x==0 then 0
    else uDekadnu (x/10) osn*osn + (x mod 10);;
    


let rec ceoDeo1 x i=
    if (i*i) > x then i-1
    else ceoDeo1 x (i+1);;
    
let ceoDeo x=ceoDeo1 x 1;;
