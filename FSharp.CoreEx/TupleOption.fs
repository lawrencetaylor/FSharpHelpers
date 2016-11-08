module Tuple.Option

type Tuple() = 

  static member ofSndOptionToOption (a , bO) = 
    match bO with
    | Some b -> Some (a, b)
    | None -> None