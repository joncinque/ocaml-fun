type 'a tree = | Leaf of 'a | Node of 'a tree * 'a tree
type direction = L | R

(* Part 1, getting a subtree from a direction list *)
let rec subtree : direction list -> 'a tree -> 'a tree option = function

    (* at the end, return the tree *)
    | [] -> fun t -> Some(t)
    | hd::tl -> function
        (* keep moving down *)
        | Node (l, _)  when hd=L-> subtree tl l
        | Node (_, r)  when hd=R-> subtree tl r
        (* either a Leaf or something else, no tree to return *)
        | _  -> None

(* Part 2, given a set of directions and an input tree, decode the message *)
let decode : direction list -> 'a tree -> 'a list option =

    (* always hold on to the root of the tree to pop back out *)
    let rec decode_helper dirs node root acc = match dirs, node with
    
        (* At the end of the directions and at a leaf, return decoded list *)
        | [], Leaf v -> Some(v::acc)
        
        (* At the end of the directions but NOT at a leaf, return None *)
        | [], _ -> None
        
        (* More directions left, and at a leaf, so add the leaf and start over*)
        | l, Leaf v -> decode_helper l root root (v::acc)
        
        (* At a node, so move down to the correct branch *)
        | hd::tl, Node (_,r) when hd=R -> decode_helper tl r root acc
        | hd::tl, Node (l,_) when hd=L -> decode_helper tl l root acc
        | _, _ -> None (* treat all cases *)
        
    (* Correctly process None or Some return types *)
    in fun dirs root -> match (decode_helper dirs root root []) with
        | Some l -> Some(List.rev l)
        | None -> None

(* Part 3, create a Huffman tree given a list of symbols and probabilities *)
let huffman : ('a * float) list -> 'a tree = fun l ->

    (* helper comparison function for sorting nodes by probability *)
    let prob_compare l r = match l,r with
        | (_,p1), (_,p2) -> let diff = p1 -. p2 in 
                            if diff > 0. then 1 else if diff = 0. then 0 else -1
                            
    (* start off with a sorted list from lowest to greatest probability *)
    in let sorted_items = List.sort prob_compare 
                          (List.rev_map (fun (e,p) -> (Leaf e, p)) l)
                          
    in let rec merge_node = function
        (* take the first two nodes in the list, assumed to be smallest,
         * combine them, then merge the new node into the main list (sorted) *)
        | (n1,p1)::(n2,p2)::tl -> 
            merge_node (List.merge prob_compare tl [Node (n1, n2),(p1+.p2)])
        | (n1,_)::_ -> n1
        | _ -> raise (Failure "Malformed list provided") (* treat all cases *)
    in merge_node sorted_items
    
    
