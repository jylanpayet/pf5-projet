open Include
open Lecture
open Affichage
open Evaluation
open Simplification
open Vars
open Sign

let read_polish (filename:string) : program = parse_to_program(lecture filename)

let print_polish (p:program) : unit = print_block (p)

let eval_polish (p:program) : unit = eval_block (p)

let simpl_polish (p:program) : program = simpl_program(p)

let vars_polish (p:program) : unit = let a,b,c = vars_program p in print_set a;print_set c

let sign_polish (e:expr) : unit = failwith "TODO"

let usage () =
  print_string "\nPolish : analyse statique d'un mini-langage\n";
  print_string "\n Usage: \n\n";
  print_string "  -reprint [filename] : Affiche le fichier Polish en synstaxe abstraite, sans les commentaires, 
  sans les espaces inutiles et toujours fonctionnnelle.\n\n";
  print_string "   -eval [filename] : Évalue le fichier Polish et affiche les expressions contenues dans chaque PRINT.\n\n";
  print_string "   -simpl [filename] : Simplifie le programme donné en paramètre, supprime les blocks de codes inutiles et toutes les opérations possibles. \n\n";
  print_string "   -vars [filename] : Affiche toutes les variables utilisées dans le programme sur une première ligne et toutes celles qui ne sont potentiellement pas initialisées sur la suivante.\n\n";
  print_string "   -sign [filename] : Arrive très bientôt. \n\n"


let main () =
  match Sys.argv with
  | [|_;"-reprint";file|] -> print_polish (read_polish file)
  | [|_;"-eval";file|] -> eval_polish (read_polish file)
  | [|_;"-simpl";file|] -> print_polish(simpl_polish(read_polish file))
  | [|_;"-vars";file|] -> vars_polish (read_polish file)
  | [|_;"-sign";file|] -> usage ()
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
