// 4.1
let rec upto = function
| 0 -> []
| n -> (upto (n - 1)) @ [n]

// 4.2
let rec downto1 = function
| 0 -> []
| n -> n :: (downto1 (n - 1))

// 4.3
let rec evenN = function
| 0 -> [0]
| n when (n % 2 = 0) -> (evenN (n - 1)) @ [n]
| n -> (evenN (n - 1)) 

// 4.10
let prefix (a: int list) (b: int list) = 
    if (a.Length > b.Length) then false else
        let rec traverse = function
            | (x::[], y::_) when (x = y) -> true
            | (x::xs, y::ys) when (x = y) -> traverse (xs , ys)
            | (_, _) -> false
        traverse(a, b);;

