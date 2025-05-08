type expr =
    Num : int -> expr
  | Bool : bool -> expr
  | Mult : expr * expr -> expr
  | And : expr * expr -> expr
  | Eq : expr * expr -> expr

(* Es: Mult (Num 3) (Bool true) è rappresentata come { Mult, { Num, 3 }, { Bool true } } *)

type res = 
    | I : int -> res
    | B : bool -> res
    | E : res

let to_string =
    function
        I n -> string_of_int n
    |   B b -> string_of_bool b
    |   E -> "error"

let rec eval: expr -> ??? =
    function
        Num n -> I n
    |   Bool b -> B b
    |   Mult(e1, e2) ->
            (match eval e1, eval e2 with
             | I n1, I n2 -> I (n1 * n2)
             | _, _ -> E)
    |   And(e1, e2) ->
            (match eval e1, eval e2 with
             | B b1, B b2 -> B (b1 && b2)
             | _, _ -> E)
    |   Eq(e1, e2) ->
            (match eval e1, eval e2 with
             | I n1, I n2 -> B (n1 == n2)
             | B b1, B b2 -> B (b1 == b2)
             | _, _ -> E)

let good = Eq (Mult (Num 3, Num 4), Num 5)
let bad = Eq (And (Bool true, Num 4), Num 5)

let _ = 
    Printf.printf "%s\n" (to_string (eval good)) ;
    Printf.printf "%s\n" (to_string (eval bad))