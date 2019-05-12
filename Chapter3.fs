// Dependencies
open System

// 3.1
type Zone =
| AM
| PM;;

let zoneToMinutes z = if (z = PM) then (12 * 60) else 0;;

// With triples
let (.<) (h1, m1, z1) (h2, m2, z2) =
    (((h1 * 60) + m1 + (zoneToMinutes z1))) < ((h2 * 60) + m2 + (zoneToMinutes z2));;

// With records
type Time = {hour:int; minute:int; zone:Zone};;
let (..<) t1 t2 = 
    (((t1.hour * 60) + t1.minute + (zoneToMinutes t1.zone))) < ((t2.hour * 60) + t2.minute + (zoneToMinutes t2.zone));;

/// 3.2
let (.+) (a:float, b:float) (c:float, d:float) = (a + c, b + d);;
let (.*) (a:float, b:float) (c:float, d:float) = ((a * c) - (b * d), (b * c) + (a * d));;

let (~~) (a:float, b:float) = ((0.00 - a) , (0.00 - b));;
let (.-) (a:float, b:float) (c:float, d:float) = (a , b) .+ ~~(c, d);;

/// 3.4
// 3.4.1
type StraightLine = float * float;;

// 3.4.2
let mirrorAcrossX line =
    let (a, b) = line
    (a, -b);;
let mirrorAcrossY line = 
    let (a, b) = line
    (-a, b);;

// 3.4.3
let lineToString line = 
    let (a, b) = line
    "y = " + (string a) + "x + " + (string b);;

 /// 3.5
 type Solution =
 | NoRoot
 | OneRoot of float
 | TwoRoots of (float * float);;

let solve (a, b, c) = 
    let discriminant = (b * b) - (4.00 * a * c)
    if (discriminant < 0.00) then
        NoRoot
    else 
        let R1 = (-b + Math.Sqrt(discriminant)) / 2.00 * a
        if (a = 0.00) then
            OneRoot(R1)
        else 
            let R2 = (-b + Math.Sqrt(discriminant)) / 2.00 * a
            TwoRoots(R1, R2)