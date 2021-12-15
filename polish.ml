open Include
open Lecture
open Affichage
open Evaluation

let read_polish (filename:string) : program = parse_to_program(lecture filename)

let print_polish (p:program) : unit = print_block (p)

let eval_polish (p:program) : unit = eval_block (p)

let usage () =
  print_string "\nPolish : analyse statique d'un mini-langage\n";
  print_string "\n Usage: \n\n   -reprint (filename:string) : Affiche le fichier Polish en synstaxe abstraite, sans les commentaires, 
  sans les espaces inutiles et toujours fonctionnnelle.\n\n   -eval (filename:string) : Évalue le fichier Polish et affiche les expressions contenues dans chaque PRINT.\n\n"

let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
