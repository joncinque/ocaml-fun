(* "nth": force evaluation on the next element until the counter hits 0 *)
let rec nth : 'a inflist -> int -> 'a =
    function | Cons (h,t) ->
        fun n -> if n > 0 then nth (Lazy.force t) (n-1) else h

(* "take": build up the returned list as long as counter i > 0 *)
let rec take : int -> 'a inflist -> 'a list =
    fun i -> function | Cons (h,t) when i > 0 -> h::(take (i-1) (Lazy.force t))
                      | _                     -> []

(* "drop": move on to the next element in the list if counter i > 0, 
 * otherwise return the current infinite list *)
let rec drop : int -> 'a inflist -> 'a inflist =
    fun i -> function | Cons (h,t) when i > 0 -> drop (i-1) (Lazy.force t)
                      | _ as l                -> l

(* "map2": match both infinite lists with for head/tail values, then construct
 * the next element of the list *)
let rec map2 : ('a -> 'b -> 'c) -> 'a inflist -> 'b inflist -> 'c inflist =
    fun f l1 l2 -> match l1, l2 with
        | Cons (h1,t1), Cons(h2,t2) ->
                Cons (f h1 h2, lazy (map2 f (Lazy.force t1) (Lazy.force t2)))
