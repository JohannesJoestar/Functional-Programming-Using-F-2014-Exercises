open System;;

// 2.1
let f (n: int) = ((n % 2 = 0) && (n % 3 = 0)) && (n % 5 <> 0);;

// 2.2
let rec pow = function
| (s, 0) -> ""
| (s, n) -> s + pow (s, (n - 1));;

// 2.3
let isIthChar (str: string, i, ch) = str.[i] = ch;;

// 2.4
let rec occFromIth (str: string, i: int, ch: char) = 
    if (i >= str.Length) then 0 else (if (str.[i] = ch) then 1 else 0) + occFromIth (str, (i + 1), ch);

// 2.5
let occInString (str: string, ch: char) = occFromIth (str, 0, ch);;

// 2.6
let notDivisible (d, n) = (n % d <> 0);;
let notDivisible2 = function
| (d, n) when (d < 0 || n < 0) -> true
| (d, n) -> (n % d <> 0);;

// 2.11
let VAT n x = x + ((x * n) / 100);;
let unVAT n x = x / ((100 + n)/ 100);;