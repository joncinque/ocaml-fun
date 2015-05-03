(* Makes things fun *)
let () = Random.self_init ()

(* Helper to parse through all of the words in a given file *)
let all_words_of_file filewords fp num =
    (* Helper regexes used in parsing *)
    let space_regex = Str.regexp "[ \t\n]+" in
    let dot_regex = Str.regexp "\\." in

    let rec get_words_of_file acc fp =

        let get_words_in_line line =
            let rec start_stop_helper linewords = function
                | [] -> linewords
                | hd :: tl -> 
                    match Str.search_forward dot_regex hd 0 with
                    | v -> start_stop_helper 
                        (("START",num)::("STOP",num)::(hd,num)::linewords) tl
                    | exception Not_found -> start_stop_helper ((hd,num)::linewords) tl in
            let words = Str.split space_regex line in
            start_stop_helper [] words in

        (* Go through every line *)
        match input_line fp with
        | line -> get_words_of_file (List.fold_left (fun acc x -> x :: acc) acc
                      (get_words_in_line line)) fp
        | exception End_of_file -> acc in

    (* Append to the existing list of words *)
    ("STOP",num) :: (get_words_of_file (("START", num)::filewords) fp)

(* List implementation *)
let print_list_helper (w, slist) = Printf.printf "%s, ()\n" w 

let build_list wordlist = 
    let compare_entry_helper (w1, _) (w2, _) = compare w1 w2 in
    let lookup l w =
        match List.find (fun (x, _) -> x = w) l with
        | (w, slist) -> List.nth slist (Random.int (List.length slist))
        | exception Not_found -> raise Not_found in
    let merge_in l w s =
        match List.partition (fun (x, _) -> x = w) l with
        | [(w, s_list)],l2 -> List.merge compare_entry_helper 
                              [ (w, (List.merge compare [s] s_list)) ] l2
        | [],l2 -> List.merge compare_entry_helper [ (w, [s]) ] l2
        | _ -> raise Not_found in
    let rec build_table_helper acc wordlist =
        match wordlist with 
        | [] | [ _ ] -> acc
        | (w,_) :: (s :: _ as tl) -> build_table_helper (merge_in acc w s) tl in
    lookup (build_table_helper [] wordlist)

(* hash table implementation *)
let build_hash_table wordlist prefix =
    let rec build_prefix acc l n =
        match l with
        | [] -> (List.rev acc, "")
        | (x,_) :: xs when n = 0 -> (List.rev acc, x)
        | (x,_) :: xs -> build_prefix (x :: acc) xs (n-1) in
    let table = Hashtbl.create 1000 in
    let lookup t w = 
        let successors = Hashtbl.find_all t w in
        if List.length successors = 0 then raise Not_found
        else [List.nth successors (Random.int (List.length successors))] in
    let rec build_table t = function
        | [] | [ _ ] -> t
        | _ :: xs as l -> let (pre,suc) = build_prefix [] l prefix in
               Hashtbl.add t pre suc ; build_table t xs in
    lookup (build_table table wordlist)

(* Main walk function given a function pointer bound to a markov chain *)
let walk finder s e =
    let rec walk_helper finder s e acc =
        if s = e then s::acc
        else walk_helper finder (finder s) e (s::acc) in
    walk_helper finder s e []

(* Main for using the table implementation *)
let main_table filenames prefix b e =
    let processfile (acc,num) fn =
        let fp = open_in fn in
        let words = (all_words_of_file acc fp num) in
        let () = close_in fp in
        (words, num + 1) in
    let (wordlist,_) = List.fold_left processfile ([], 1) filenames in
    let finder = build_hash_table wordlist prefix in
    (*let finder = build_list wordlist in*)
    let words = List.rev (walk finder b e) in
    List.iter (fun w -> Printf.printf "%s " (List.hd w)) words

let () = main_table ["cookbook.txt"] 1 ["START"] ["STOP"]
