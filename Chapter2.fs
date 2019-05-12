// 2.1
let f n = ((n % 2 = 0) && (n % 3 = 0)) && (n % 5 <> 0);;

// 2.2
let rec pow = function
| (s, 0) -> ""
| (s, n) -> s + pow (s, (n - 1));;

// 2.3
let isIthChar (str: string, i, ch) = str.[i] = ch;;

// 2.4
let rec occFromIth (str: string, i, ch) = 
    if (i >= str.Length) then 0 else (if (str.[i] = ch) then 1 else 0) + occFromIth (str, (i + 1), ch);

// 2.5
let occInString (str, ch) = occFromIth (str, 0, ch);;

// 2.6
let notDivisible (d, n) = (n % d <> 0);;
let notDivisible2 = function
| (d, n) when (d < 0 || n < 0) -> true
| (d, n) -> (n % d <> 0);;

//// 2.7
// 1
let rec test = function
| (a, b, _) when (a > b) -> true
| (a, b, c) -> 
    notDivisible (a, c) && test ((a + 1), b, c)
// 2
let prime number =
    
    // Auxiliary recursive
    let rec check number = function
    | i -> (i > (number / 2)) || ((number % i <> 0) && (check number (i + 1)))

    check number 2
// 3
let rec nextPrime = function
| number when (prime (number + 1)) -> (number + 1)
| number -> nextPrime (number + 1)

//// 2.8
// Auxiliary: factorial
let rec bin (n, k) = 
    if k = 0 || n = k then 1
    else bin(n-1, k-1) + bin(n-1, k)

// 2.11
let VAT n x = x + ((x * n) / 100);;
let unVAT n x = x / ((100 + n)/ 100);;

// 2.12
let min check = 

    // Auxiliary: recursive count
    let rec recursive = function
    | n when ((check n) = 0) -> n
    | n -> recursive (n + 1)

    recursive 1;;