let rec harm n=
    if n==1 then [1.0]
    else harm (n-1) @ [1.0/.(float_of_int n)];;
    

let rec delioci1 n k=
    if k==n then [1]
    else if (n mod k)==0 then [k] @ (delioci1 n (k+1))
    else delioci1 n (k+1);;
    
let rec delioci n=
    if n==1 then [1]
    else delioci1 n 2;;   
    
    
let rec nadovezi lista1 lista2 n=
    if n==1 then lista1 @ lista2
    else (nadovezi lista1 lista2 (n-1)) @ lista2;;
    
    
