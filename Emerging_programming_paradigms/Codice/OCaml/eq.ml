type 'a expr =
    Num : int -> int expr
  | Bool : bool -> bool expr
  | Mult : int expr * int expr -> int expr
  | And : bool expr * bool expr -> bool expr
  | Eq : (* forall 'b *) 'b expr * 'b expr -> bool expr

type ('a, 'b) eq = 
  | Refl : (* forall 'c *) ('c, 'c) eq 

(* Dimostrazione della riflessività *)
let refl : type a. (a, a) eq = Refl

(* Dimostrazione della simmetria *)
let symm : type a b. (a, a) eq -> (b, b) eq =
    function 
        (*  L'input è Refl tipato con forall 'c. ('c, 'c) eq
            Sia 'c un tipo fissato
            Quindi      (a, b) eq = ('c, 'c) eq
            Quindi      a = 'c = b
            Quindi il mio output diventa
                (b, a) eq   ovvero  ('c, 'c) eq
        *)
        Refl -> Refl

(* Dimostrazione della transitività *)
let trans : type a b c. (a, b) eq -> (b, c) eq -> (a, c) eq = 
    fun x y ->
        match x, y with
         |  Refl, Refl -> Refl

(*
let same_type : type a b. a expr -> b expr -> bool = 
    fun e1 e2 ->
        match e1, e2 with
         |  (Num _ | Mult _), (Num _ | Mult _) -> true
         |  (Bool _ | And _ | Eq _), (Bool _ | And _ | Eq _) -> true
         |  _, _ -> false
*)

(* Restituisco: None se non hanno lo stesso tipo, Some Refl se hanno lo stesso tipo *)
let same_type : type a b. a expr -> b expr -> (a expr, b expr) eq option = 
    fun e1 e2 ->
        match e1, e2 with
         |  (Num _ | Mult _), (Num _ | Mult _) -> Some Refl
         |  (Bool _ | And _ | Eq _), (Bool _ | And _ | Eq _) -> Some Refl
         |  _, _ -> None

let cast : type a b. (a, b) eq -> a -> b =
    function
        (*  Poichè Refl : ('c, 'c) eq
            si ha   (a, b) eq = ('c, 'c) eq
            quindi  a = b = 'c
            e il tipo di ritorno a -> b diventa 'c -> 'c
        *)
        Refl -> (fun x -> x)

let equal : type a b. a expr -> b expr -> bool = 
    fun e1 e2 ->
        match same_type e1 e2 with
            Some p -> e1 = cast e2
        else
            None -> false