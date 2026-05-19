datatype expr = Var of string | Not of expr | And of expr * expr | Or of expr * expr;

fun findval (x: string) [] = false
  | findval x ((a, b) :: t) = 
      if a = x then b else findval x t
      
fun evaluate env exp = 
    case exp of
      And (e1, e2) => (evaluate env e1) andalso (evaluate env e2)
    | Or (e1, e2)  => (evaluate env e1) orelse (evaluate env e2)
    | Not e1       => not (evaluate env e1)
    | Var x        => findval x env;


fun add_to_all x [] = []
  | add_to_all x (inner_list :: rest) = 
      (x :: inner_list) :: add_to_all x rest;

fun create_combinations [] = [[]]
  | create_combinations (h :: t) = 
      let 
          val rest_combinations = create_combinations t
      in
          (add_to_all (h, true) rest_combinations) @ (add_to_all (h, false) rest_combinations)
      end;

fun table vars exp = 
    let
      val all_envs = create_combinations vars
    in
      map (fn single_env => (rev single_env, evaluate single_env exp)) all_envs
    end;