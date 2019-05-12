open System

// 4.10
let prefix (a: int list) (b: int list) = 
    if (a.Length > b.Length) then false else
        let rec traverse = function
            | (x::[], y::_) when (x = y) -> true
            | (x::xs, y::ys) when (x = y) -> traverse (xs , ys)
            | (_, _) -> false
        traverse(a, b);;

