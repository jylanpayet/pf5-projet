open Include

let rec print_ind (ind:int) : string =
  match ind with 
  | 0 -> ""
  | _ ->  "  " ^ (print_ind (ind-1))

let print_op (o:op) : string =
  match o with 
  | Add -> "+ " | Sub -> "- " | Mul -> "* " | Div -> "/ " | Mod -> "% " 

let print_comp (c:comp) : string = 
  match c with
  | Eq -> "= "| Ne -> "<> " | Lt -> "< " | Le -> "<= " | Gt -> "> " | Ge -> ">= " 

let rec print_expr (e:expr) : string =
  match e with 
  | Num(i) -> string_of_int (i)
  | Var(v) -> v
  | Op(a,b,c) -> let n = print_op a
      in n ^ (print_expr b) ^ " " ^ (print_expr c)
                                    
let print_cond (c:cond) : string =
  match c with 
  | (x,y,z) -> print_expr x ^ " " ^ print_comp y ^ print_expr z
               
let print_block (prog:program) : unit = 
  let rec aux prog (ind:int) =
    match prog with 
    | [] -> ()
    | (x,y)::r -> match y with 
      | Set(name,expr) -> (Printf.printf "%s%s := %s\n" (print_ind ind)  name (print_expr expr)); (aux r ind)
      | Read(name) -> Printf.printf "%sREAD %s\n" (print_ind ind) name; (aux r ind)
      | Print(expr) -> Printf.printf "%sPRINT %s\n" (print_ind ind) (print_expr expr); (aux r ind)
      | If(cond,block1,block2) -> if (List.length block2) = 0 then ((Printf.printf "%sIF %s\n" (print_ind ind) (print_cond cond)); (aux block1 (ind+1)); (aux r ind))
          else ((Printf.printf "%sIF %s\n" (print_ind ind) (print_cond cond)); (aux block1 (ind+1)); (Printf.printf "%sELSE\n" (print_ind ind));(aux block2 (ind+1));(aux r ind))
      | While(cond,block) ->(Printf.printf "%sWHILE %s\n" (print_ind ind) (print_cond cond)); (aux block (ind+1)); (aux r ind)
  in aux prog 0