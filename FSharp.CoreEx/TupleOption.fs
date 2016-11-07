module Tuple = 

  let ofSndOptionToOption (a , bO) = 
    match bO with
    | Some b -> Some (a, b)
    | None -> None