module Chapters

module Chapter6 = 
    let rec drop n list = 
        match n, list with
        | _, [] -> []
        | 0, list -> list
        | n, _:: t  -> drop (n-1) t

    let rec _and xs = 
        match xs with
        | [] -> true
        | x::xs ->  x && _and xs

    let rec append xs ys = 
        match xs, ys with
        | [], [] -> []
        | _, [] -> xs
        | [], _ -> ys
        | x::xs, ys -> x :: append xs ys 
            
    let rec concat xss = 
        match xss with
        | [] -> []      
        | (x::xs) -> append x (concat xs)

    let rec replicate n x = 
        match n with
        | 0 -> []
        | n -> x :: replicate (n-1) x

    let rec nth n xs = 
        match n, xs with
        | _, [] -> None
        | 0, x::_ -> x |> Some
        | n, _::xs -> nth (n-1) xs  

    let rec elem el xs = 
        match xs with
        | [] -> false
        | x::xs -> if x = el then true else elem el xs 

module Chapter7 =

    let rec map f xs = 
        match xs with
        | [] -> []
        | x::xs ->
            (f x) :: map f xs

    let rec filter pred xs = 
        match xs with
        | [] -> []
        | x::xs ->
            if pred x then 
                x :: filter pred xs 
            else filter pred xs 

    let rec foldr f v xs =
        match xs with
        | [] -> v
        | x::xs -> 
            f x (foldr f v xs)
