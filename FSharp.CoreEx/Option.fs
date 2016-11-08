module Option.Core

type Option() =

  static member fallback f o = 
    match o with
    | Some v -> v
    | None -> f