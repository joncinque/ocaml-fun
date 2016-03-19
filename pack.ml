let pack l =                                                                    
    let rec pack_helper pl elems elem = function                                
        | [] -> elems :: pl                                                     
        | hd :: tl -> if elem = hd then pack_helper pl (elem :: elems) elem tl 
                      else pack_helper (elems :: pl) [hd] hd tl                 
    in match l with                                                             
    | [] -> []                                                                  
    | hd :: tl -> pack_helper [] [hd] hd tl                                     
                                                                                
let encode l =                                                                  
    let rec encode_helper = function                                            
        | [] -> (' ', 0)                                                        
        | (hd :: _) as l -> (hd, List.length l)                                 
    in List.map encode_helper l                                                 
                                                                                
let oneshot_pack l =                                                            
    let rec pack_helper pl elem occ = function                                  
        | [] -> (elem, occ) :: pl                                               
        | hd :: tl -> if elem = hd then pack_helper pl elem (occ+1) tl          
                      else pack_helper ((elem, occ) :: pl) hd 1 tl              
    in match l with                                                             
    | [] -> []                                                                  
    | hd :: tl -> pack_helper [] hd 1 tl                                        
                                                                                
let test = ['a';'a';'a';'a';'b';'c';'c';'a';'a';'d';'e';'e';'e';'e']            
let first_test = pack test
let () = List.iter (fun l -> Printf.printf "[" ;                                
                    List.iter (Printf.printf "%c ") l ;                         
                    Printf.printf "] ") first_test                              
let second_test = encode (pack test)                                            
let packed = oneshot_pack test                                                  
let () = List.iter (fun (e,o) -> Printf.printf "(%c, %d) " e o) packed          
let () = List.iter (fun (e,o) -> Printf.printf "(%c, %d) " e o) second_test
