(* Get the cumulative distribution *)                                           
let cdf_approx x =                                                              
    let z x =                                                                   
        let a0 = 2.490895 in                                                    
        let a2 = 1.466003 in                                                    
        let a4 = -0.024393 in                                                   
        let a6 = 0.178257 in                                                    
        (a0 +. a2 *. (x ** 2.) +. a4 *. (x ** 4.) +. a6 *. (x ** 6.)) ** -1. in 
    let t =                                                                     
        let p = 0.2316419 in                                                    
        1. /. (1. +. p *. x) in                                                 
    let b1 = 0.319381530 in                                                     
    let b2 = -0.356563782 in                                                    
    let b3 = 1.781477937 in                                                     
    let b4 = -1.821255978 in                                                    
    let b5 = 1.330274429 in                                                     
    1. -. (z x) *. (b1 *. t +. b2 *. (t ** 2.) +. b3 *. (t ** 3.) +. b4 *. (t ** 4.) +. b5 *. (t *. 5.))
                                                                                
let cdf_iter x =                                                                
    let double_fac x =                                                          
        let rec aux c x =                                                       
            if x > 0. then aux (c *. x) (x -. 2.) else c in                     
        aux 1. x                                                                
    in let rec do_sum c iter =                                                  
        if iter > 100. then c                                                   
        else do_sum (c +. x ** (2. *. iter +. 1.) /. (double_fac (2. *. iter +. 1.))) (iter +. 1.)
    in let pi = acos (-1.) in                                                   
    0.5 +. 1. /. (sqrt(2. *. pi)) *. exp(-0.5 *. x**2.) *. (do_sum 0. 0.)       
                                                                                
(* Calculate the Black-Scholes European call option price *)                    
let blackscholes maturity spot strike rate vol =                                
    let d1 = 1. /. (vol *. sqrt(maturity)) *. (log(spot /. strike) +. (rate +. 0.5 *. (vol ** 2.)) *. maturity) in
    let d2 = d1 -. vol *. sqrt(maturity) in                                     
    cdf_iter(d1) *. spot -. cdf_iter(d2) *. strike *. exp( -1. *. rate *. maturity)
                                                                                
let () = print_float (blackscholes 0.5 42. 40. 0.1 0.21)
