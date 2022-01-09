open Include

type env = (name * int) list

let rec eval_expr (e:expr) (env:env) : int =
  match e with 
  | Num(x) -> x
  | Var(x) -> begin try List.assoc x env with Failure e -> failwith "Cette variable n'existe pas." end
  | Op(a,b,c) -> match a with 
    | Add -> ((eval_expr b env) + (eval_expr c env))
    | Sub -> ((eval_expr b env) - (eval_expr c env)) 
    | Mul -> ((eval_expr b env)  * (eval_expr c env))
    | Div -> begin try ((eval_expr b env) / (eval_expr c env)) with Failure e -> failwith "Division par 0 impossible." end
    | Mod -> begin try ((eval_expr b env) mod (eval_expr c env)) with Failure e -> failwith "Mod par 0 impossible." end

let eval_cond (c:cond) (env:env) : bool =
  match c with 
  | (expr1,Eq,expr2) -> (eval_expr expr1 env) = (eval_expr expr2 env)
  | (expr1,Ne,expr2) -> (eval_expr expr1 env) <> (eval_expr expr2 env) 
  | (expr1,Lt,expr2) -> (eval_expr expr1 env) < (eval_expr expr2 env)
  | (expr1,Le,expr2) -> (eval_expr expr1 env) <= (eval_expr expr2 env) 
  | (expr1,Gt,expr2) -> (eval_expr expr1 env) > (eval_expr expr2 env)
  | (expr1,Ge,expr2) -> (eval_expr expr1 env) >= (eval_expr expr2 env)


let eval_block (prog:program) : unit =
  let a = 
  let rec aux prog (env:env) : env =
    match prog with 
    | [] -> env
    | (x,y)::r -> match y with 
      | Set(name,expr) -> (aux r ((name,eval_expr expr env)::env))
      | Read(name) -> print_string name; print_string "?\n"; let b = read_int() in aux r ((name,b)::env)
      | Print(expr) -> (Printf.printf "%d\n"(eval_expr expr env));aux r env
      | If(cond,block1,block2) -> if (eval_cond cond env) then let a = aux block1 env in aux r a  else let b = aux block2 env in aux r b
      | While(cond,block) -> if eval_cond cond env then let a = aux block env in aux ((x,y)::r) a else aux r env
  in aux prog [] 
  in ()