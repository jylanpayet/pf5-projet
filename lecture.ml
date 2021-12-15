open Include

let rec indentation_ligne (list:ligne) : int = 
  match list with 
  | [] -> 0
  | x::r -> if (String.equal x "") then 1 + indentation_ligne r else 0

let ignore_comment (list:ligne) : bool =
  match List.hd list with 
  | "COMMENT" -> false 
  | _ -> true

(** Lit tout les lignes du fichier et les transforment en le tout en type: list_de_lignes *)
let rec copy_lines fichier l n = 
  try
    let x = String.split_on_char ' ' (input_line fichier) in let y = indentation_ligne x in
    if (y mod 2) = 0 then let h = List.filter (function z -> (z <> "")) x in    
      if ignore_comment h then copy_lines fichier ((n,(y/2),h)::l) (n+1) else copy_lines fichier l n else failwith "Mauvaise indentation" 
  with End_of_file -> close_in fichier;List.rev l

(** Ouvre le fichier et démarre la lecture *)
let lecture (filename:string) : list_de_lignes = 
  let fichier = open_in filename in copy_lines fichier [] 1;;

let parse_operateur (st:string) =
  match st with
  |"-" -> Sub 
  |"+" -> Add
  |"*" -> Mul
  |"/" -> Div
  |"%" -> Mod
  |_ -> failwith "Opérateur inconnu." 

let est_operateur (st:string) : bool = 
  match st with 
  |"-" |"+" |"*" |"/" |"%" -> true
  | _ -> false


let not_int (st:string) =
    try 
    (ignore (int_of_string st));false
    with Failure e -> true
 

let parse_expr (ligne:ligne) : (expr) =
  let v,w = 
    let rec aux_parse_expr (ligne:ligne) : (expr*ligne) =
      match ligne with 
      | [] -> failwith "Expression incorrect."
      | x::r -> if est_operateur x then let a,b = aux_parse_expr r in let c,d = aux_parse_expr b in Op((parse_operateur x), a, c),d
          else 
            try 
              Num(int_of_string x),r 
            with Failure e -> Var (x),r 
    in aux_parse_expr ligne 
  in if(List.length w) = 0 then v else failwith "Expression incorrect."

let verif_comp (st:string) : bool = 
  match st with 
  |"=" |"<" |">" |"<>" |">="| "<=" -> true
  | _ -> false

let parse_comp (st:string) : comp = 
  match st with 
  |"=" -> Eq |"<" -> Lt |">"  -> Gt |"<>" -> Ne |">=" -> Ge |"<=" -> Le
  | _ -> failwith "Le comparateur existe pas."

let parse_cond (ligne:ligne): cond = 
  try 
    let rec aux_parse_cond (ligne:ligne) (second:ligne) : (ligne*ligne*string) = 
      match ligne with 
      | [] -> failwith "Condition impossible."
      | x::r -> if verif_comp x then second,r,x
          else aux_parse_cond r (second @ [x]) 
    in let g,d,x = aux_parse_cond (List.tl ligne) []
    in let a= parse_expr g in let b = parse_expr d in (a,parse_comp(x),b)
  with Failure e -> failwith "Condition incorrect."

let parse_set (ligne:ligne) =
  try 
    let a = List.hd ligne
    in 
    if not_int a then
      let b = List.nth ligne 1
      in if b = ":=" then let c = List.tl ligne in let d = parse_expr (List.tl c) 
        in Set(a,d) else failwith "Probleme d'affectation."
    else failwith "Un nombre ne peut pas être une variable."
  with Failure e -> failwith "Set incorrect."

let parse_read (ligne:ligne)= 
  if (List.length ligne) = 2 then 
    let a = List.nth ligne 1 in 
    if not_int a then
      Read(List.nth ligne 1)
    else failwith "Un nombre ne peut pas être une variable."
  else failwith "Read impossible."
 
let parse_print (ligne:ligne) =
  try 
    Print(parse_expr (List.tl ligne))
  with Failure e -> failwith "Print impossible car l'expression est fausse."

let rec parse_block (res:list_de_lignes) (list:list_de_lignes) (p:int) =
  match list with 
  |[] -> (res,list)
  |(x,y,z)::r -> if y>p then parse_block (res@[(x,y,z)]) r p else (res,list)

let parse_to_program (list:list_de_lignes) : program = 
  let rec parse_instr (list:list_de_lignes) = 
    match list with 
    | [] -> []
    |(x,y,z)::r -> match List.hd z with
      | "READ" -> (x,parse_read z)::parse_instr(r)
      | "PRINT" -> (x,parse_print z)::parse_instr(r)
      |"IF" -> let a,b = parse_block [] r y 
                in 
                begin
                match b with 
                |[] -> (x,If(parse_cond z,parse_instr a,[]))::parse_instr (b)
                |(i,j,k)::l -> if (List.hd k = "ELSE") then let c,d = parse_block [] l j in 
                               (x,If(parse_cond z,parse_instr a,parse_instr c))::parse_instr(d)
                               else (x,If(parse_cond z,parse_instr a,[]))::parse_instr(b)
                end
      | "WHILE" -> let a,b = parse_block [] r y in (x,While(parse_cond z,parse_instr a))::parse_instr(b)
      | _ -> (x,parse_set z)::parse_instr(r)
  in parse_instr list

