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

// 4.4

// 4.5
let rec rmodd = function
| [] -> []
| h::t when (h % 2 = 0) -> h :: (rmodd t)
| _::t -> (rmodd t)

// 4.6
let rec rmeven = function
| [] -> []
| h::t when (h % 2 <> 0) -> h :: (rmeven t)
| _::t -> (rmeven t)

// 4.7
let rec multiplicity x = function
| [] -> 0
| h::t -> 
    (multiplicity x t) + if (h = x) then 1 else 0

// 4.10
let prefix (a: int list) (b: int list) = 
    if (a.Length > b.Length) then false else
        let rec traverse = function
            | (x::[], y::_) when (x = y) -> true
            | (x::xs, y::ys) when (x = y) -> traverse (xs , ys)
            | (_, _) -> false
        traverse(a, b);;

// 4.12
let rec sum p = function
| [] -> 0
| h::t -> (match (p h) with
            | true -> h
            | false -> 0) + (sum p t)

// 4.15
let rec revrev =

    // Helper: reverse
    let rec reverse = function
    | []   -> []
    | h::t -> (reverse t) @ [h]

    // Helper: nested reverse
    let rec auxiliary = function
    | []   -> []
    | h::t -> (auxiliary t) @ [reverse h]

    function | l -> auxiliary l

