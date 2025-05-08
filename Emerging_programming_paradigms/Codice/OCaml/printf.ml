(* Esempio di una funzione printf-like:     val to_string : 'a spec -> 'a -> string *)

(* Le specifiche corrispondono ai vari %.. della printf *)
type 'a spec =
  | D : int spec
  | F : float spec
  | S : string spec
  | Pair : 'a spec * 'b spec -> ('a * 'b) spec
  | List: 'a spec -> ('a list) spec

let rec to_string : type a. a spec -> a -> string =
    function
        D -> string_of_int
     |  F -> string_of_float
     |  S -> (fun x -> x)
     |  Pair(s1, s2) ->
            (*  Poichè l'input è Pair(s1, s2):
                s1 : 'a spec
                s2 : 'b spec
                a spec = ('a * 'b) spec
                Quindi      a = 'a * 'b
                e quindi devo restituire una funzione   'a * 'b -> string
            *)
        (function (x,y) ->      (* x : 'a       y : 'b *)
            "<" ^ to_string s1 x ^ "," ^ to_string s2 y ^ ">")
    | List(s) ->
        (function l ->
            "[" ^ String.concat "," (List.map (to_string s) l) ^ "]")

let _ =
    let example_spec = List (Pair (D,F)) in
    print_string (to_string example_spec [(3,3.5);(0,1.14)])