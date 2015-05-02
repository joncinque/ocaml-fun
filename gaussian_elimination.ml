(* Simple base functions for Gaussian elimination *)
let scale_row (row : float array) (scale : float) : float array = 
    Array.map (fun f -> f *. scale) row

let subtract_rows (row1 : float array) (row2 : float array) : float array =
    Array.mapi (fun i f -> f -. row2.(i)) row1

let dot (v1 : float array) (v2 : float array) : float =
    Array.mapi (fun i f -> f *. v2.(i)) v1 |> Array.fold_left (+.) 0.

(* To avoid writing "Array.length - 1" all over the place *)
let last_index (input_array : 'a array) = Array.length input_array - 1

(* Helper when mapping through arrays, bind the first two parameters so that the
 * third one can be passed in by Array.map *)
let eliminate_helper (reduce_index : int) 
                     (top_row : float array)
                     (iter_row : float array) = 
    let scale = iter_row.(reduce_index) /. top_row.(reduce_index) in
    scale_row top_row scale |> subtract_rows iter_row

(* Currently assumes that there are no zeroes in the coefficient matrix *)
let elimination_phase (coef_matrix : float array array) : float array array =

    (* Main work to eliminate from the top row down, while this is purely 
     * functional, it is NOT tail-recursive because of the calls to "sub",
     * "map", and "append" *)
    let rec eliminate (index : int) (matrix : float array array) =
        if Array.length matrix = 1 then matrix
        else begin
            let top_row = matrix.(0) in
            let rest = Array.sub matrix 1 (last_index matrix) in
            let reduced = Array.map (eliminate_helper index top_row) rest in
            eliminate (index + 1) reduced |> Array.append [|top_row|]
        end in
    eliminate 0 coef_matrix

(* Just a copy of elimination phase which goes backwards as an alternative
 * implementation instead of using back substitution *)
let back_elimination_phase (coef_matrix : float array array) 
                                        : float array array =
    let rec eliminate (index : int) (matrix : float array array) =
        let bottom_row = matrix.(last_index matrix) in
        let normalized_row = scale_row bottom_row (1. /. bottom_row.(index)) in
        if Array.length matrix = 1 then [| normalized_row |]
        else begin
            let rest = Array.sub matrix 0 (last_index matrix) in
            let reduced = Array.map (eliminate_helper index bottom_row) rest in
            Array.append (eliminate (index - 1) reduced) [| normalized_row |]
        end in
    eliminate (last_index coef_matrix.(0) - 1) coef_matrix

(* Starts from the bottom to substitute all results into the equation, only
 * possible for matrices in echelon form *)
let back_substitution_phase (coef_matrix : float array array) 
                                         : float array array =
    let fold_helper (row : float array) 
                    ((index, result) : int * (float array)) = 
                    
        (* Dot the current result vector with the "equation" to get the
         * equation with substituted values, subtract it from the "right side" 
         * of the equation, then divide by the coefficient for the new value *)
        let substituted = dot (Array.sub row 0 (last_index row)) result in
        let right = row.(last_index row) in
        let coef = row.(index) in
        let () = result.(index) <- (right -. substituted) /. coef in
        (index - 1, result) in
        
    (* Construct a result vector from scratch *)
    let zeroes = Array.make (Array.length coef_matrix) 0. in
    
    (* Fold over the coefficient matrix and build up the result vector *)
    let (_, result) = Array.fold_right fold_helper coef_matrix 
                      (last_index coef_matrix, zeroes) in
                      
    Array.map (fun x -> [| x |]) result

let gauss (a : float array array) : float array array =
    a |> elimination_phase |> back_substitution_phase

(* Testing *)
let v1 = [| 4. ; 3. ; 2. |]
let v2 = [| 2. ; 1. ; 5. |]
let () = Printf.printf "%f\n" @@ dot v1 v2

let a = [| 
    [|  6. ; -4. ;  1. ; -14. |] ;
    [| -4. ;  6. ; -4. ;  36. |] ;
    [|  1. ; -4. ;  6. ;   6. |] ;
|]

(* Helper to print matrices *)
let matrix_printer a =
    print_string "[ ";
    Array.iter (fun v -> print_string "[ "; 
                         Array.iter (Printf.printf "%f ") v;
                         print_endline " ]") a;
    print_endline " ]"
let () = matrix_printer a

(* Test using elimination phases *)
let e = elimination_phase a
let () = matrix_printer e

let b = back_elimination_phase e
let () = matrix_printer b

(* Test using full gaussian elimination *)
let g = gauss a
let () = matrix_printer g
