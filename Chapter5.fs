// 5.1
let filter f l = List.foldBack (fun e a -> if (f e) then (e :: a) else a) l []

// 5.2
let revrev list = 
    List.fold (fun acc v -> [(List.fold (fun a e -> e :: a) [] v)] @ acc) [] list