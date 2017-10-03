open Convert

type 'a hash_pointer = { hash : string; data : 'a }
type 'a blockchain = Nil | Data of 'a hash_pointer

module type Hashable = sig 
  type t
  val hash : t -> string
end

module Make_blockchain(Data : Hashable) = struct

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

  let rec verify = function
    | Nil -> true
    | Node({ ptr = Null  ; data = Data d }, _) -> true
    | Node({ ptr = Null  ; data = Empty }, _) -> 
        raise (Invalid_argument "Block has neither pointer nor data, and is not null")
    | Node({ ptr = s ; data = _ }, n) -> begin
      let () = hash_of_node n |> string_of_hash_pointer |> print_endline in
      if s = hash_of_node n then verify n else false
    end

  let empty = Nil

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

module String_blockchain = Make_blockchain(struct
    type t = string
    let hash s = s
end)

let () =  
  let s = String_blockchain.empty in
  let s_list = "s1" :: "s2" :: "s3" :: "s4" :: "s5" :: [] in
  let s1 = String_blockchain.add_list s s_list in
  String_blockchain.verify s1 |> Printf.printf "%B"
