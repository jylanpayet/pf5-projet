open Include

module Init = Set.Make(String)

let print_set s = (Init.iter (Printf.printf "%s ") s); print_endline ""

let vars_expr (e:expr) = 
  let rec aux e list =
    match e with 
    | Num(x) -> list
    | Var(x) -> Init.add x list
    | Op(x,a,b) -> let y,z = aux a list,aux b list in Init.union y z 
  in aux e Init.empty

let vars_cond (c:cond) =
  let (expr1,x,expr2) = c in Init.union (vars_expr expr1) (vars_expr expr2)

let vars_program (p:program) = 
  let rec aux p all vi prob =
    match p with 
    | [] -> (all,vi,prob)
    | (x,y)::r -> match y with 
      | Read(name) -> aux r (Init.add name all) (Init.add name all) prob
      | Set(name,expr) -> let a = (vars_expr expr) in let b = Init.diff a vi in aux r (Init.union b (Init.add name all)) (Init.add name all) (Init.union b prob)
      | Print(expr) -> let a = (vars_expr expr) in let b = Init.diff a vi in aux r (Init.union b all) vi (Init.union b prob)
      | If(cond,block1,block2) -> let a = (vars_cond cond) in let b = Init.diff a vi in 
            let (c,d,e),(f,g,h) = (aux block1 (Init.union b all) vi (Init.union b prob)),(aux block2 (Init.union b all) vi (Init.union b prob)) 
            in let j,k = (Init.union c f),(Init.union e h) 
            in aux r (Init.union b (Init.union j all)) (Init.inter d g) (Init.union b (Init.union k prob))
      | While(cond,block) -> let a = (vars_cond cond) in let b = Init.diff a vi in 
            let (c,d,e) = aux block (Init.union b all) vi (Init.union b prob) 
            in let (f,g,h) = aux r (Init.union b all) vi (Init.union b prob) 
            in ((Init.union c f),g,(Init.union e h))
  in aux p (Init.empty) (Init.empty) (Init.empty)

