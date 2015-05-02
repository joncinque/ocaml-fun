let powerset l =
    let rec loop acc l =
        let gen_sets acc elem = (List.map (fun x -> elem::x) acc) @ acc
        in match l with
        | hd::tl -> loop (gen_sets acc hd) tl
        | [] -> acc
    in List.map List.rev (loop [[]] l) (* reverse for neatness *)
