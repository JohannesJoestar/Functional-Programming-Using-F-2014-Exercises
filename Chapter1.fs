// Dependencies
open System

// 1.1
let g n = (n + 4);

// 1.2
let h x y = Math.Sqrt((x * x) + (y * y));;

// 1.3
let g' = fun n -> (n + 4);;
let h' = fun x y -> Math.Sqrt((x * x) + (y * y));;

// 1.4
let rec f = function
| 0 -> 0
| n -> f (n - 1) + n;;

// 1.5
let rec fibo = function
| 0 -> 0
| 1 -> 1
| n -> fibo (n - 1) + fibo (n - 2);;

// 1.6
let rec sum = function
| (m, 0) -> 0
| (m, n) -> (m + n) + sum (m, (n - 1));;