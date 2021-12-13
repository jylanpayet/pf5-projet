open Include
open Lecture
open Affichage
open Evaluation

let read_polish (filename:string) : program = parse_to_program(lecture filename)

let print_polish (p:program) : unit = print_block (p)

let eval_polish (p:program) : unit = eval_block (p)

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
