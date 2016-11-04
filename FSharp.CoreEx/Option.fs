module Option

  let fallback f o = 
    match o with
    | Some v -> v
    | None -> f