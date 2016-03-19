(*
 * ast.ml
 *
 *     Abstract syntax tree.
 *
 *)

type id = string

type expr =
   | Expr_unit
   | Expr_bool   of bool
   | Expr_int    of int
   | Expr_id     of id
   | Expr_define of id * expr
   | Expr_if     of expr * expr * expr
   | Expr_lambda of id list * expr list
   | Expr_apply  of expr * expr list

let rec ast_of_sexpr sx = match sx with
   | Sexpr.Expr_list sx -> ast_of_sexpr_list sx
   | Sexpr.Expr_atom a -> ast_of_atom a

and ast_of_atom = function
   | Sexpr.Atom_unit -> Expr_unit
   | Sexpr.Atom_bool b -> Expr_bool b
   | Sexpr.Atom_int i  -> Expr_int i
   | Sexpr.Atom_id s   -> Expr_id s

and id_of_expr = function
   | Sexpr.Atom_id s -> s
   | _ -> failwith "Unknown type in id_of_expr"

and id_of_atom = function
   | Sexpr.Expr_atom a -> id_of_expr a
   | _ -> failwith "Trying to get id from non-atom"

and ast_of_define = function
   | (Sexpr.Expr_atom s) :: xs :: [] -> 
           Expr_define (id_of_expr s, ast_of_sexpr xs)
   | _ -> failwith "Bad define expression"

and ast_of_lambda = function
   | (Sexpr.Expr_list ids) :: xs -> 
           Expr_lambda(List.map id_of_atom ids, List.map ast_of_sexpr xs)
   | _ -> failwith "Bad lambda"

and ast_of_apply = function
   | exp :: exps -> 
           Expr_apply(ast_of_sexpr exp, List.map ast_of_sexpr exps)
   | _ -> failwith "Bad apply"

and ast_of_if = function
   | exp1 :: exp2 :: exp3 :: _ ->
           Expr_if(ast_of_sexpr exp1, ast_of_sexpr exp2, ast_of_sexpr exp3)
   | _ -> failwith "Bad if"

and ast_of_sexpr_list = function
    | (Sexpr.Expr_atom a::sx) as l->
        begin match a with
        | Sexpr.Atom_id("define") -> ast_of_define sx
        | Sexpr.Atom_id("lambda") -> ast_of_lambda sx
        | Sexpr.Atom_id("if") -> ast_of_if sx
        | _ -> ast_of_apply l
        end
    | _ -> failwith "bad list"
                     

let string_of_ast ast =
   let sprintf  = Printf.sprintf in  (* to make the code cleaner *)
   let spaces n = String.make n ' ' in
   let rec string_of_ids id_lst = 
      match id_lst with
         | [] -> ""
         | [id] -> id
         | h :: t -> h ^ " " ^ (string_of_ids t)
   in

   let rec iter ast indent =
      let string_of_exprs e_list =
         (List.fold_left (^) ""
             (List.map
                 (fun e -> "\n" ^ iter e (indent + 2))
                 e_list))
      in
      match ast with
         | Expr_unit    -> sprintf "%sUNIT"       (spaces indent) 
         | Expr_bool b  -> sprintf "%sBOOL[ %b ]" (spaces indent) b
         | Expr_int  i  -> sprintf "%sINT[ %d ]"  (spaces indent) i
         | Expr_id   id -> sprintf "%sID[ %s ]"   (spaces indent) id
         | Expr_define (id, e) -> 
              sprintf "%sDEFINE[%s\n%s ]" 
                 (spaces indent) id (iter e (indent + 2))
         | Expr_if (test_clause, then_clause, else_clause) ->
              sprintf "%sIF[\n%s\n%s\n%s ]"
                 (spaces indent) 
                 (iter test_clause (indent + 2))
                 (iter then_clause (indent + 2))
                 (iter else_clause (indent + 2))
         | Expr_lambda (ids, body) ->
              sprintf "%sLAMBDA[(%s)%s ]"
                 (spaces indent)
                 (string_of_ids ids)
                 (string_of_exprs body)
         | Expr_apply (operator, operands) ->
              sprintf "%sAPPLY[\n%s%s ]"
                 (spaces indent)
                 (iter operator (indent + 2))
                 (string_of_exprs operands)
   in
      "\n" ^ iter ast 0 ^ "\n"


let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let rec loop () =
      let sexpr  = Parser.parse Lexer.lex lexbuf in
         match sexpr with
            | None -> ()
            | Some s ->
                 let expr = ast_of_sexpr s in
                    Printf.printf "%s\n" (string_of_ast expr); 
                    flush stdout;
                    loop ()
   in
      loop ()


