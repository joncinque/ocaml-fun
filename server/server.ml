open Unix 
open Str 

(* For clearer debugging *)
let () = Printexc.record_backtrace true 

let print_error e =
  let msg = Printexc.to_string e
  and stack = Printexc.get_backtrace () in
  Printf.eprintf "Error: %s%s\n" msg stack;
  Unix.out_channel_of_descr stdout |> flush 

(* Similar to Unix.establish_server, but does not fork a new process.
 * This is to persist the Hashtbl between calls to the server. *)
let establish_server server_fun sockaddr =
  (* The Hashtbl can be replaced with any other data provider *)
  let dbtbl = Hashtbl.create 1000 in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let sock = Unix.socket domain Unix.SOCK_STREAM 0 
  in Unix.bind sock sockaddr ;
  Unix.listen sock 1 ;
  while true do
    let (s, caller) = Unix.accept sock in
    let inchan = Unix.in_channel_of_descr s 
    and outchan = Unix.out_channel_of_descr s 
    in server_fun dbtbl inchan outchan ;
    Unix.close s
  done 

(* Parses args and starts up server with given processing function *)
let main_server server_func =
  if Array.length Sys.argv < 3 then Printf.eprintf "usage: <host> <portnum>\n"
  else 
    try
      let local_address = (Unix.gethostbyname(Sys.argv.(1))).h_addr_list.(0) in
      let port = int_of_string Sys.argv.(2) in
      let inet_address = Unix.ADDR_INET(local_address, port)
      in establish_server server_func inet_address
    with e -> print_error e

(* Main function to process curl requests, takes in a Hashtbl and request.
 * Example format of http_request is:
 * GET /get?key=somekey HTTP/1.1
 * or
 * GET /set?somekey=someval HTTP/1.1 *)
let process_request dbtbl http_request = 
  let req_list = Str.split (Str.regexp "[ \t]+") http_request in
  let q = List.nth req_list 1 in
  let parse_regex = Str.regexp "/\\(.*\\)\?\\(.*\\)=\\(.*\\)" in
  if Str.string_match parse_regex q 0 then
    match Str.matched_group 1 q with
      | "set" -> begin
          Hashtbl.replace dbtbl (Str.matched_group 2 q) (Str.matched_group 3 q) ;
          "Set successful"
        end
      | "get" -> begin
          if (Str.matched_group 2 q) = "key" then 
            Hashtbl.find dbtbl (Str.matched_group 3 q)
          else invalid_arg "Misunderstood get"
        end
      | s -> invalid_arg ("Misunderstood command: " ^ s)
  else invalid_arg ("Misunderstood HTTP query: " ^ q)
 
let key_value_service dbtbl in_channel out_channel =
  try 
    (* Only take the first line of the curl request, all that matters *)
    let i_string = input_line in_channel in
    let o_string = "\n" ^ (process_request dbtbl i_string) ^ "\n" in
    let _ = output_string out_channel o_string in
    flush out_channel
  with e -> print_error e

let () =
  try 
    Unix.handle_unix_error main_server key_value_service
  with e -> print_error e ; exit 0
