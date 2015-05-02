(* Given module, extended to get list of nodes and edges *)
module type Graph_sig = sig
  type node
  type t
  type colors = White | Gray | Black
  type 'a state

  val of_adjacency : (node * node list) list -> t
  val breadth_first_fold : t -> node -> 'b -> ('b -> node -> 'b) -> 'b state
  val discovery_times : 'a state -> (node * int) list
  val colors : 'a state -> (node * colors) list
  val data : 'a state -> 'a

  (* Test helpers *)
  val nodes : t -> node list
  val edges : t -> (node * node) list
end

module type GRAPH = sig
  module type Ord=sig
    type t val compare : t -> t -> int
  end

  module type S = sig
    include Graph_sig
  end 

  module Make (N : Ord) : S with type node = N.t
end

module Graph : GRAPH = struct
    module type Ord = sig
        type t
        val compare : t -> t -> int
    end

    module type S = sig
        include Graph_sig
    end

    module Make (N : Ord) : S with type node = N.t = struct

        type node = N.t
        type t = { nodes : node list ; edges : (node * node) list }
        type colors = White | Gray | Black

        (* Helper module for easy hashing when storing fold state *)
        module N_map = Map.Make(N)

        type 'a state = { acc    : 'a ; 
                          colors : colors N_map.t ;
                          times  : int N_map.t }

        (* Simple comparator for storing edges sorted by first node, then second
         * node in the underlying Graph structure *)
        let edge_compare (l1,l2) (r1,r2) =
            if (N.compare l1 r1 = 0) then N.compare l2 r2 else N.compare l1 r1

        (* The underlying graph structure is just lists, and we're not doing any
         * error checking for bad adjacency lists. *)
        let of_adjacency l =
            let rec add_helper (nodes, edges) = function
                | [] -> (nodes, edges)
                | (n, es)::ns -> 
                    (* Keep things sorted at each stage using List.merge *)
                    let cur_edges = List.map (fun e -> (n,e)) es in
                    add_helper ((List.merge N.compare [n] nodes),
                                (List.merge edge_compare cur_edges edges)) ns in
            let nodes, edges = add_helper ([],[]) l in
            { nodes = nodes ; edges = edges }

        (* Helper functions in fold *)

        (* Gives all nodes adjacent to the current *)
        let get_adjacent { edges } n =
            let node_adder n acc (l,r) = if n = l then r :: acc else acc in
            List.rev (List.fold_left (node_adder n) [] edges)

        (* Creates an initial color state for folding, all is White *)
        let init_colors nodes = List.fold_left 
            (fun acc n -> N_map.add n White acc) N_map.empty nodes

        (* Creates an initial time state for folding, first node is at 0 *)
        let init_times n = N_map.add n 0 N_map.empty

        let breadth_first_fold g n acc f = 

            (* Use a queue to know which nodes to process *)
            let q = Queue.create () in
            let () = Queue.push n q in

            (* Helper function when going through the adjacent nodes of the
             * visited node: only add White nodes to the process queue and 
             * color them Gray. We bind the queue and discovery time for use
             * with List.fold_left on the list of adjacent nodes *)
            let node_adder q t (time_map, color_map) n = 
                if N_map.find n color_map = White
                then let () = Queue.push n q in 
                    (N_map.add n t time_map, N_map.add n Gray color_map)
                else (time_map, color_map) in

            (* Pop from the queue and create the new state until we're done *)
            let rec fold_helper g q f s =
                match Queue.pop q with
                | exception Queue.Empty -> s
                | n -> 
                    (* Set up needed values *)
                    let acc = f s.acc n in
                    let times = s.times in
                    let t = N_map.find n times in
                    let colors = N_map.add n Black s.colors in
                    let adj = get_adjacent g n in
                    
                    (* Add necessary nodes to the queue and update maps *)
                    let (times, colors) = 
                        List.fold_left (node_adder q (t+1)) (times, colors) adj
                        
                    (* Recurse *)
                    in fold_helper g q f { acc    = acc ; 
                                           colors = colors ; 
                                           times  = times }
                                           
            in fold_helper g q f { acc    = acc ; 
                                   colors = init_colors g.nodes ;
                                   times  = init_times n }


        let data { acc } = acc

        let colors { colors } = 
            N_map.fold (fun k v acc -> (k,v)::acc) colors []

        let discovery_times { times } = 
            N_map.fold (fun k v acc -> (k,v)::acc) times []

        (* Test helpers *)
        let nodes { nodes } = nodes
        let edges { edges } = edges

    end
end

(* Testing *)
module G : Graph.S with type node = Char.t = Graph.Make (Char)
let g : G.t = G.of_adjacency
  [ 'r', ['v' ; 's']       ;
    'v', ['r']             ;
    's', ['r' ; 'w']       ;
    'w', ['x' ; 't']       ;
    't', ['w' ; 'x' ; 'u'] ;
    'x', ['w' ; 't' ; 'y'] ;
    'u', ['t' ; 'y']       ;
    'y', ['x' ; 'u']       ;
  ]

let () = print_endline "Node list"
let ns = G.nodes g
let () = List.iter (Printf.printf "%c,") ns
let () = print_endline ""

let () = print_endline "Edge list"
let es = G.edges g
let () = List.iter (fun (n1,n2) -> Printf.printf "%c -> %c\n" n1 n2) es

let () = print_endline "Fold result"
let s = G.breadth_first_fold g 's' [] (fun acc x -> x :: acc)
let d = List.rev (G.data s)
let () = List.iter (Printf.printf "%c\n") d

let c = G.colors s
let () = print_endline "Colors"
let string_of_color = function
    | G.White -> "White"
    | G.Black -> "Black"
    | G.Gray -> "Gray"
let () = List.iter (fun (n,c) -> Printf.printf "(%c, %s)\n" n (string_of_color c)) c

let t = G.discovery_times s
let () = print_endline "Discovery times"
let () = List.iter (fun (n,t) -> Printf.printf "(%c,%d)\n" n t) t
