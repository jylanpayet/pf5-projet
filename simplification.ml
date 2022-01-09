open Include

let is_num (e:expr) : bool =
  match e with 
  | Num(x) -> true 
  | _ -> false 

let get_constante (e:expr) : int =
  match e with
  | Num(x) -> x
  | _ -> failwith "L'expression entrée n'est pas un entier." 


let calcul_constante (o:op) (i:int) (j:int) : expr =
  match o with 
  | Add -> Num(i + j)
  | Sub -> Num(i - j)
  | Mul -> Num(i * j)
  | Div -> if (j=0) then Op(Div,Num(i),Num(j)) else Num(i / j)
  | Mod -> if (j=0) then Op(Mod,Num(i),Num(j)) else Num(i mod j)

let simpl_operande (e:expr) : expr =
  match e with 
  | Op(a,b,c) ->
      if (is_num b) && (is_num c) then (calcul_constante a (get_constante b) (get_constante c)) 
      else if (is_num b) then 
        if ((((get_constante b)=0)) && (a=Add)) || (((get_constante b)=1) && (a=Mul)) then c
        else if ((get_constante b)=0) && (a=Mul || a=Div || a=Mod) then Num(0) 
        else Op(a,b,c)  
      else if (is_num c) then 
        if (((get_constante c)=0) && (a=Add || a=Sub)) || (((get_constante c)=1) && (a=Mul || a=Div)) then b
        else if ((get_constante c)=0) && (a=Mul) then Num(0)
        else Op(a,b,c)
      else Op(a,b,c)
  | _ -> failwith "L'argument rentrée n'est pas une opérande."

let rec simpl_expr (e:expr) : expr =    
  match e with 
  | Num(x) -> Num(x)
  | Var(x) -> Var(x)
  | Op(a,b,c) -> 
      if (is_num b) && (is_num c) then (calcul_constante a (get_constante b) (get_constante c))
      else if (is_num b) then let x=simpl_expr (c) in simpl_operande (Op(a,b,x))
      else if (is_num c) then let x=simpl_expr (b) in simpl_operande (Op(a,x,c))  
      else let x,y=simpl_expr (b), simpl_expr (c) in simpl_operande (Op(a,x,y))

let bool_condition (c:cond) : bool = 
  match c with 
  | (expr1,Eq,expr2) -> get_constante expr1 = get_constante expr2
  | (expr1,Ne,expr2) -> get_constante expr1 <> get_constante expr2
  | (expr1,Lt,expr2) -> get_constante expr1 < get_constante expr2
  | (expr1,Le,expr2) -> get_constante expr1 <= get_constante expr2
  | (expr1,Gt,expr2) -> get_constante expr1 > get_constante expr2
  | (expr1,Ge,expr2) -> get_constante expr1 >= get_constante expr2

let simpl_condition (c:cond) : (cond*bool) = 
  let (e1,x,e2) = c in let a,b=(simpl_expr e1),(simpl_expr e2) in ((a,x,b)),((is_num a) && (is_num b))
   
let simpl_program (p:program) =
  let rec aux p res = 
    match p with 
    | [] -> res
    | (x,y)::r -> match y with 
      | Set(name,expr) -> aux r (res@[(x,Set(name,simpl_expr expr))])
      | Read(name) -> aux r (res@[(x,y)])
      | Print(expr) -> aux r (res@[(x,Print(simpl_expr expr))])
      | If(cond,block1,block2) -> let a,b = (simpl_condition cond) in if b then if (bool_condition a) then aux r (res@(aux block1 [])) else aux r (res@(aux block2 [])) else aux r (res@[(x,If(a,(aux block1 []),(aux block2 [])))])
      | While(cond,block) -> let a,b = (simpl_condition cond) in if b && not(bool_condition a) then aux r res else aux r (res@[(x,While(a,(aux block [])))]) 
  in aux p [] 