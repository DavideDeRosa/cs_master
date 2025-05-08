type 'a expr =
    Num : int -> int expr
  | Bool : bool -> bool expr
  | Mult : int expr * int expr -> int expr
  | And : bool expr * bool expr -> bool expr
  | Eq : (* forall 'b *) 'b expr * 'b expr -> bool expr

(* Es: Mult (Num 3) (Bool true) è rappresentata come { Mult, { Num, 3 }, { Bool true } } *)

let rec eval: type.a a expr -> a =
 (* Il tipo dell'input è              a expr *)
 (* Il tipo da dare in onput è        a      *)

    function
        (*  Siamo nel caso Num! Num è dichiarato come segue:
            
            Num : int -> int expr

            Quindi:     n : int
            L'input (Num n) ha tipo     int expr

            Quindi:     a expr = int expr
            Per iniettività dei costruttori di GADTs:
                        a = int
            
            Quindi: il tipo di ritorno che era a ora è int! *)
        Num n -> n

        (*  Siamo nel caso Bool! Bool è dichiarato come segue:
            
            Bool : bool -> bool expr

            Quindi:     b : bool
            L'input (Bool b) ha tipo     bool expr

            Quindi:     a expr = bool expr
            Per iniettività dei costruttori di GADTs:
                        a = bool
            
            Quindi: il tipo di ritorno che era a ora è bool! *)
    |   Bool b -> b

        (*  Siamo nel caso Mult! Mult è dichiarato come segue:
            
            Mult : int expr * int expr -> int expr

            Quindi:     e1 : int expr, e2 : int expr
            L'input (Mult(e1, e2)) ha tipo     int expr

            Quindi:     a expr = int expr
            Per iniettività dei costruttori di GADTs:
                        a = int
            
            Quindi: il tipo di ritorno che era a ora è int! 
            Inoltre: la eval ha tipo    'a expr -> 'a 
            Quindi: eval e1 ha tipo int, eval e2 ha tipo int *)
    |   Mult(e1, e2) -> eval e1 * eval e2

    |   And(e1, e2) -> eval e1 && eval e2

        (*  Siamo nel caso Eq! Eq è dichiarato come segue:
            
            Eq : forall 'b. 'b' expr * 'b' expr -> bool expr

            Sia 'b un tipo ignoto ma fissato
            Quindi:     e1 : 'b' expr, e2 : 'b' expr
            L'input (Eq(e1, e2)) ha tipo     bool expr

            Quindi:     a expr = bool expr
            Per iniettività dei costruttori di GADTs:
                        a = bool
            
            Quindi: il tipo di ritorno che era a ora è bool! 
            Inoltre: la eval ha tipo    'a expr -> 'a 
            Quindi: eval e1 ha tipo 'b', eval e2 ha tipo 'b' *)
    |   Eq(e1, e2) -> eval e1 == eval e2

let good = Eq (Mult (Num 3, Num 4), Num 5)
let bad = Eq (And (Bool true, Num 4), Num 5)

let _ = 
    Printf.printf "%s\n" (to_string (eval good)) ;
    Printf.printf "%s\n" (to_string (eval bad))