(* General signature declarations *)
module type Set_sig = sig
    type element (*The type of elements of a set*)
    type t (*The type of a set of elements*)

    exception Empty_set

    val empty : t (*The empty set*)
    val add : t -> element -> t (*Add an element*)
    val remove : t -> element -> t * element (*Remove an element*)
    val fold : t -> 'a -> ('a -> element -> 'a) -> 'a (*Visit elements in order*)
    val contains : t -> element -> bool (*Test for membership*)
    val size : t -> int (*Cardinality of a set*)
    val union : t -> t -> t (*The union of two sets*)
    val intersection : t -> t -> t (*The intersection of two sets*)
    val min_element : t -> element (*The minimum value of the set*)
    val max_element : t -> element (*The maximum value of the set*)
    val of_list : element list -> t (*Construct set from unordered list*)
end

module type Ordered_type_sig = sig
    type t
    val compare : t -> t -> int
end

(* Set signature declaration *)
module type SET = sig
    module type Ordered_type = Ordered_type_sig

    module type S = Set_sig

    module Make (Ord : Ordered_type) : S with type element = Ord.t
end

(* Set implementation *)
module Set : SET = struct
    module type Ordered_type = Ordered_type_sig

    module type S = Set_sig

    module Make (Ord: Ordered_type) : S with type element = Ord.t = struct
        type element = Ord.t
        type t = | Empty | Node of t * element * t

        exception Empty_set

        let empty = Empty
        let rec add s e = 
            match s with
            | Empty -> Node (Empty, e, Empty)
            | Node (_,v,_) as n when e = v -> n
            | Node (l,v,r) -> if e < v then Node(add l e,v,r) 
                              else Node(l,v, add r e)
        let rec min_element = function
            | Empty -> raise Empty_set
            | Node (Empty,v,_) -> v
            | Node (l,_,_) -> min_element l

        let rec max_element = function
            | Empty -> raise Empty_set
            | Node (_,v,Empty) -> v
            | Node (_,_,r) -> max_element r

        let rec contains s e =
            match s with
            | Empty -> false
            | Node (_,v,_) when v = e -> true
            | Node (l,v,_) when e < v -> contains l e
            | Node (_,v,r) when e > v -> contains r e
            | Node (_,_,_) -> false

        let rec fold s acc f =
            match s with
            | Empty -> acc
            | Node (l, v, r) -> fold r (f (fold l acc f) v) f

        let rec preorder_fold s acc f =
            match s with
            | Empty -> acc
            | Node (l, v, r) -> preorder_fold r (preorder_fold l (f acc v) f) f

        let remove s e =
            let rec remove_helper s e = 
                match s with
                | Empty -> raise Not_found
                | Node (l,v,r) when e < v -> Node (remove_helper l e, v, r)
                | Node (l,v,r) when e > v -> Node (l, v, remove_helper r e)
                | Node (Empty,v,Empty) when e = v -> Empty
                | Node (l,v,Empty) when e = v -> l
                | Node (Empty,v,r) when e = v -> r
                | Node (l,v,r) when e = v ->
                        let submin = min_element r in 
                        Node(l, submin, remove_helper r submin)
                | Node (_,_,_) -> raise Not_found
            in (remove_helper s e, e)

        let size s = fold s 0 (fun acc _ -> acc + 1)

        let of_list l =
            let rec add_helper s = function
                | [] -> s
                | x :: xs -> add_helper (add s x) xs
            in add_helper empty l

        let list_of s =
            List.rev(fold s [] (fun acc x -> x :: acc))

        let union s1 s2 = preorder_fold s2 (preorder_fold s1 empty add) add

        let intersection s1 s2 =
            let rec add_helper acc l1 l2 =
                match l1,l2 with
                | [], _ | _, [] -> acc
                | x1 :: xs1, x2 :: xs2 -> 
                        if x1 = x2 then add_helper (add acc x1) xs1 xs2
                        else if x1 > x2 then add_helper acc l1 xs2
                        else add_helper acc xs1 l2
            in add_helper empty (list_of s1) (list_of s2)
    end
end

module Ordered_string = struct
    type t = string
    let compare = Pervasives.compare
end

(* Testing *)
module String_set = Set.Make(Ordered_string)
let e = String_set.empty
let e = String_set.add e "3"
let e = String_set.add e "one"
let e = String_set.add e "two"
let e = String_set.add e "blah"
let e = String_set.add e "ocaml"
let e = String_set.add e "ocaml"
let e = String_set.add e "0"
let () = String_set.fold e () (fun _ x -> print_endline x)

let (e,_) = String_set.remove e "one"
let () = String_set.fold e () (fun _ x -> print_endline x)

let s = String_set.of_list ["1"; "2"; "3"; "4"; "5"; "0"]
let () = String_set.fold s () (fun _ x -> print_endline x)

let () = print_endline (string_of_int (String_set.size e))
let () = print_endline (string_of_int (String_set.size String_set.empty))

let u = String_set.union e s
let () = String_set.fold u () (fun _ x -> print_endline x)

let i = String_set.intersection e s
let () = String_set.fold i () (fun _ x -> print_endline x)
