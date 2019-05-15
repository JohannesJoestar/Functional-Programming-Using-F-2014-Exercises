// 5.1
let filter f l = List.foldBack (fun e a -> if (f e) then (e :: a) else a) l []