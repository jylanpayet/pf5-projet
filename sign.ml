open Include

type sign = Neg | Zero | Pos | Error

type list_sign = (sign) list 

module V = Map.Make(String)
module S = Set.Make(String)

let print_list l =
  let rec aux l res =
    match l with
    | [] -> res
    | x::r -> match x with 
              | Zero -> aux r ("zero "^res)
              | Pos -> aux r ("pos "^res)
              | Neg -> aux r ("neg "^res)
              | Error -> aux r ("error "^res)
  in aux l ""

let parse_list_to_set l =
  let rec aux l s =
    match l with
    | [] -> s 
    | x :: r -> match x with 
                | Zero -> aux r (S.add "Zero" s)
                | Pos -> aux r (S.add "Pos" s)
                | Neg -> aux r (S.add "Neg" s)
                | Error -> aux r (S.add "Error" s)
  in aux l S.empty

let parse_set_to_list s = S.elements s

let concat list res =
  let rec aux list res =
    match list with 
    | [] -> res
    | x::r -> if List.mem x res then aux r res else aux r (x::res)
  in aux list res

let sign_num (i:int) : sign =
  if i = 0 then Zero 
  else if i > 0 then Pos 
  else Neg

(** Cas : Expression *)

let is_error l1 l2 = ((List.length l1)=1 && (List.hd l1)=Error) || ((List.length l2)=1 && (List.hd l2)=Error)

(** Add ------------------------------------------------------------------------ *)
let add_zero l1 l2 l3 = if (List.mem Zero l1) && (List.mem Zero l2) then concat [Zero] l3 else l3
let add_pos l1 l2 l3 = if (List.mem Pos l1 && List.mem Pos l2) || (List.mem Zero l1 && List.mem Pos l2) || (List.mem Pos l1 && List.mem Zero l2) then concat [Pos] l3 else l3
let add_neg l1 l2 l3 = if (List.mem Neg l1 && List.mem Neg l2) || (List.mem Zero l1 && List.mem Neg l2) || (List.mem Neg l1 && List.mem Zero l2) then concat [Neg] l3 else l3
let add_posneg l1 l2 l3 = if (List.mem Neg l1 && List.mem Pos l2) || (List.mem Pos l1 && List.mem Neg l2)  then concat [Pos;Zero;Neg] l3 else l3
let add_error l1 l2 l3 = if List.mem Error l1 || List.mem Error l2 then concat [Error] l3 else l3
let sign_add l1 l2 = if is_error l1 l2 then [Error] else let a = add_zero l1 l2 [] in let b = add_pos l1 l2 a in let c = add_neg l1 l2 b in let d = add_posneg l1 l2 c in add_error l1 l2 d
(** ---------------------------------------------------------------------------- *)

(** Sub ------------------------------------------------------------------------ *)
let sub_zero l1 l2 l3 = if List.mem Zero l1 && List.mem Zero l2 then concat [Zero] l3 else l3
let sub_pos l1 l2 l3 = if (List.mem Pos l1 && List.mem Neg l2) || (List.mem Zero l1 && List.mem Neg l2) || (List.mem Pos l1 && List.mem Zero l2) then concat [Pos] l3 else l3
let sub_neg l1 l2 l3 = if (List.mem Neg l1 && List.mem Pos l2) || (List.mem Zero l1 && List.mem Pos l2) || (List.mem Neg l1 && List.mem Zero l2) then concat [Neg] l3 else l3
let sub_posneg l1 l2 l3 = if (List.mem Pos l1 && List.mem Pos l2) || (List.mem Neg l1 && List.mem Neg l2) then concat [Pos;Zero;Neg] l3 else l3
let sub_error l1 l2 l3 = if List.mem Error l1 || List.mem Error l2 then concat [Error] l3 else l3
let sign_sub l1 l2 = if is_error l1 l2 then [Error] else let a = sub_zero l1 l2 [] in let b = sub_pos l1 l2 a in let c = sub_neg l1 l2 b in let d = sub_posneg l1 l2 c in sub_error l1 l2 d
(** ---------------------------------------------------------------------------- *)

(** Mul ------------------------------------------------------------------------ *)
let is_mul_zero l1 l2 = ((List.length l1)=1 && (List.hd l1)=Zero) || ((List.length l2)=1 && (List.hd l2)=Zero)
let mul_zero l1 l2 l3 = if List.mem Zero l1 || List.mem Zero l2 then concat [Zero] l3 else l3
let mul_pos l1 l2 l3 = if (List.mem Pos l1 && List.mem Pos l2) then concat [Pos] l3 else l3
let mul_neg l1 l2 l3 = if (List.mem Neg l1 && List.mem Neg l2) || (List.mem Neg l1 && List.mem Pos l2) || (List.mem Pos l1 && List.mem Neg l2) then concat [Neg] l3 else l3
let mul_error l1 l2 l3 = if List.mem Error l1 || List.mem Error l2 then concat [Error] l3 else l3
let sign_mul l1 l2 = if is_error l1 l2 then [Error] else if is_mul_zero l1 l2 then [Zero] else let a = mul_zero l1 l2 [] in let b = mul_pos l1 l2 a in let c = mul_neg l1 l2 b in mul_error l1 l2 c
(** ---------------------------------------------------------------------------- *)

let is_error_2 l= (List.mem Error l || List.mem Zero l)

(** Div ------------------------------------------------------------------------ *)
let is_div_zero l = ((List.length l)=1 && (List.hd l)=Zero)
let div_zero l1 l2 l3 = if List.mem Zero l1 then concat [Zero] l3 else l3
let div_pos l1 l2 l3 = if (List.mem Pos l1 && List.mem Pos l2) || (List.mem Neg l1 && List.mem Neg l2)  then concat [Pos] l3 else l3
let div_neg l1 l2 l3 = if (List.mem Neg l1 && List.mem Pos l2) || (List.mem Pos l1 && List.mem Neg l2) then concat [Neg] l3 else l3
let div_error l1 l2 l3 = if List.mem Error l1 || List.mem Error l2 || List.mem Zero l2 then concat [Error] l3 else l3
let sign_div l1 l2 = if is_error l1 l2 || is_div_zero l2 then [Error] else if is_div_zero l1 && is_error_2 l2 then [Zero;Error] else if is_div_zero l1 then [Zero] else let a = div_zero l1 l2 [] in let b = div_pos l1 l2 a in let c = div_neg l1 l2 b in div_error l1 l2 c
(** ---------------------------------------------------------------------------- *)

(** Mod ------------------------------------------------------------------------ *)
let is_mod_zero l = ((List.length l)=1 && (List.hd l)=Zero)
let mod_zero l1 l2 l3 = if List.mem Zero l1 then concat [Zero] l3 else l3
let mod_poszero l1 l2 l3 = if (List.mem Pos l1 && List.mem Pos l2) || (List.mem Pos l1 && List.mem Neg l2) then concat [Pos;Zero] l3 else l3
let mod_negzero l1 l2 l3 = if (List.mem Neg l1 && List.mem Pos l2) || (List.mem Neg l1 && List.mem Neg l2) then concat [Neg;Zero] l3 else l3
let mod_error l1 l2 l3 = if List.mem Error l1 || List.mem Error l2 || List.mem Zero l2 then concat [Error] l3 else l3
let sign_mod l1 l2 = if is_error l1 l2 || is_mod_zero l2 then [Error] else if is_mod_zero l1 && is_error_2 l2 then [Zero;Error] else if is_mod_zero l1 then [Zero] else let a = mod_zero l1 l2 [] in let b = mod_poszero l1 l2 a in let c = mod_negzero l1 l2 b in div_error l1 l2 c
(** ---------------------------------------------------------------------------- *)

let sign_operande (o:op) l1 l2 =
  match o with
  | Add -> sign_add l1 l2 
  | Sub -> sign_sub l1 l2
  | Mul -> sign_mul l1 l2
  | Div -> sign_div l1 l2
  | Mod -> sign_mod l1 l2

let sign_expr (e:expr) env = 
  let rec aux e env res = 
    match e with 
    | Num(x) -> (sign_num x)::res
    | Var(x) -> if (V.mem x env) then let a = (V.find x env) in concat a res else failwith "Une variable n'a pas été initialisée correctement." 
    | Op(x,a,b) -> let c,d = aux a env res, aux b env res in  sign_operande x c d
  in aux e env []

(** Cas : Condition *)

(**  ------------------------------------------------------------------------------ *)
let cond_lt_neg l1 l2 s = if(S.mem "Neg" l1 && ((S.mem "Neg" l2) || (S.mem "Zero" l2) || (S.mem "Pos" l2) )) then S.add "Neg" s else s
let cond_lt_pos l1 l2 s = if S.mem "Pos" l1 && S.mem "Pos" l2 then S.add "Pos" s else s
let cond_lt_zero l1 l2 s = if S.mem "Zero" l1 && S.mem "Pos" l2 then S.add "Zero" s else s

let cond_lt_neg2 l1 l2 s = if S.mem "Neg" l2 && S.mem "Neg" l1 then S.add "Neg" s else s
let cond_lt_pos2 l1 l2 s = if S.mem "Pos" l2 && ((S.mem "Neg" l2) || (S.mem "Zero" l2) || (S.mem "Pos" l2)) then S.add "Pos" s else s
let cond_lt_zero2 l1 l2 s = if S.mem "Zero" l2 && S.mem "Neg" l2 then S.add "Zero" s else s

(**  ------------------------------------------------------------------------------ *)
let sign_eq l1 l2 =
  let x,y = parse_list_to_set l1, parse_list_to_set l2 in let a = (S.inter x y) in let b = (S.diff x a) in let c = (S.diff y a) in if (S.is_empty a) then false,a,b,c else true,a,b,c

let sign_ne l1 l2 =
  let x,y = parse_list_to_set l1, parse_list_to_set l2 in let a = (S.inter x y) in let b = (S.diff x a) in let c = (S.diff y a) in if (S.is_empty a) then true,a,b,c else false,a,b,c

let sign_lt l1 l2 = 
  let x,y = parse_list_to_set l1, parse_list_to_set l2 in let a,b =  S.empty,S.empty in let c,d = cond_lt_neg x y a, cond_lt_neg2 x y b 
  in let e,f = cond_lt_pos x y c ,cond_lt_pos2 x y d in let i,j = cond_lt_zero x y e, cond_lt_zero2 x y f in failwith "TODO"

(**  ------------------------------------------------------------------------------ *)

let sign_cond (c:cond) env = 
  let (expr1,x,expr2) = c 
  in match x with 
    | Eq -> failwith "TODO"
    | Ne -> failwith "TODO"
    | Lt -> failwith "TODO"
    | Gt -> failwith "TODO"
    | Ge -> failwith "TODO"
    | Le -> failwith "TODO"
