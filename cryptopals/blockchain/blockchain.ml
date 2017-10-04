open Convert

module type Hashable = sig 
  type t
  val hash : t -> string
  val string_of_t : t -> string
end

module Make_blockchain_first(Data : Hashable) = struct

  type hash_pointer = Null | Ptr of string
  type hash_data = Empty | Data of Data.t
  type block = { ptr : hash_pointer ; data : hash_data }
  type t = Nil | Node of block * t

  let string_of_hash_pointer = function
    | Null -> ""
    | Ptr s -> s

  let hash_of_node = function
    | Nil -> Null
    | Node({ ptr = Ptr s ; data = Data d }, _) -> Ptr (s ^ Data.hash d)
    | Node({ ptr = Null  ; data = Data d }, _) -> Ptr (Data.hash d)
    | Node({ ptr = Ptr _ ; data = Empty  } , _) -> 
        raise (Invalid_argument "Block has a hash pointer without data, impossible")
    | Node({ ptr = Null  ; data = Empty  }, _) -> 
        raise (Invalid_argument "Block has neither pointer nor data, impossible")

  let string_of_hash_pointer = function
    | Null -> ""
    | Ptr s -> s

  let rec verify = function
    | Nil -> true
    | Node({ ptr = Null  ; data = Data d }, _) -> true
    | Node({ ptr = Null  ; data = Empty }, _) -> 
        raise (Invalid_argument "Block has neither pointer nor data, and is not null")
    | Node({ ptr = s ; data = _ }, n) -> begin
      let () = hash_of_node n |> string_of_hash_pointer |> print_endline in
      if s = hash_of_node n then verify n else false
    end

  let empty = Null

  let add h d = match h with
    | Nil -> begin
      let new_node = Node({ ptr = Null; data = Data d}, h) in
      Node({ ptr = hash_of_node new_node; data = Empty }, new_node)
    end
    | Node({ ptr = s; data = Empty }, n) -> begin
      let new_node = Node({ ptr = s; data = Data d}, n) in 
      Node({ ptr = hash_of_node new_node ; data = Empty }, new_node) 
    end
    | Node({ ptr = _; data = Data _ }, _) -> 
        raise (Invalid_argument "Head of list has data, something's wrong")

  let rec add_list h = function
    | d :: ds -> add_list (add h d) ds
    | [] -> h

end

module Make_blockchain(Data : Hashable) = struct

  (** Core functionality *)
  type hash_pointer = Null | Ptr of string * node
  and node = hash_pointer * Data.t

  let hash_of_node = function
    | (Ptr (h, _), d) -> h ^ Data.hash d
    | (Null, d) -> Data.hash d

  let rec verify = function
    | Null -> true
    | Ptr (h, ((ptr, _) as n)) -> if h = hash_of_node n then verify ptr else false

  let empty = Null

  let add h d = let n = (h, d) in Ptr (hash_of_node n, n) 

  let rec add_list h = function
    | d :: ds -> add_list (add h d) ds
    | [] -> h

  (** Printing functionality *)
  let string_of_ptr = function
    | Null -> "Null"
    | Ptr (h, _) -> "Hash: [" ^ h ^ "]"

  let string_of_data d = "Data: [" ^ Data.string_of_t d ^ "]"

  let string_of_node num ptr d = "Node " ^ string_of_int num ^ ": [" ^ 
    string_of_ptr ptr ^ ", " ^ string_of_data d ^ "]"

  let print_chain ptr = 
    let rec print_ptr num = function
      | Null -> ()
      | Ptr (_, n) -> print_node num n
    and print_node num (ptr, d) = 
      print_endline (string_of_node num ptr d) ; print_ptr (num + 1) ptr
    in string_of_ptr ptr |> print_endline ; print_ptr 1 ptr

  let rec print_chain_old = function
    | Null -> print_endline "Null"
    | Ptr (_, (next, d)) as ptr -> begin
      print_endline ("Node: [" ^ string_of_ptr ptr ^ ", " ^ string_of_data d ^ "]") ; 
      print_chain_old next
    end
end


module String_blockchain = Make_blockchain(struct
    type t = string
    let hash s = s
    let string_of_t s = s
end)

let () =  
  let s = String_blockchain.empty in
  let s_list = "s1" :: "s2" :: "s3" :: "s4" :: "s5" :: [] in
  let s1 = String_blockchain.add_list s s_list in
  let () = String_blockchain.verify s1 |> Printf.printf "%B\n" in
  let () = String_blockchain.print_chain_old s1 in
  String_blockchain.print_chain s1
