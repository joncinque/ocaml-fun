(* Base types *)
type dict = Node of bool * (char * dict) list
let empty = Node(false, [])

(* Is the word present in the dictionary *)
let present d w = 
    let rec loop d w i =
        let rec find_char l w i =
            match l with
            | (c,Node(p,l))::xs when w.[i] = c -> 
                    if String.length w = i + 1 then p
                    else loop (Node(p,l)) w (i+1) 
            | _::xs -> find_char xs w i
            | [] -> false
        in match d with
        | Node (_, l) -> find_char l w i in
    loop d w 0

(* Insert the given word into the dictionary *)
let insert d w = 
    let rec loop d w i =
        let rec get_dict_of_char l w i =
            match l with
            | (c, Node(p,l))::xs when c = w.[i] -> 
                    if String.length w = i + 1 then (c, Node(true, l))::xs
                    else (c, loop (Node(p,l)) w (i+1))::xs
            | x::xs -> x :: (get_dict_of_char xs w I)
            | [] -> if String.length w = i + 1 then [(w.[i], Node(true, []))]
                    else [(w.[i], loop empty w (i+1) )]
        in match d with
        | Node(p, l) -> Node(p, get_dict_of_char l w i) in
    loop d w 0

let d = empty
let d = insert d "one"
let d = insert d "on"
let d = insert d "two"
let d = insert d "to"

let () =Printf.printf "%B\n" (present d "on")
let () =Printf.printf "%B\n" (present d "once")
let () =Printf.printf "%B\n" (present d "one")
let () =Printf.printf "%B\n" (present d "onus")
let () =Printf.printf "%B\n" (present d "two")
let () =Printf.printf "%B\n" (present d "tor")
