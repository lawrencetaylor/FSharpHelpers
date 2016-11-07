module Tuple

  let createWith f a = (a, f a)
  let diagonal a = createWith id a

  let mapFst f (a, b) = (f a, b)
  let mapSnd f (a, b) = (a, f b)
  let mapFstOnSndPredicate p f (a, b) = 
    match p b with
    | true -> mapFst f (a, b)
    | false -> (a, b)

  let fst = fst
  let snd = snd

  let both (a, b) =  a && b

  // let ofSndResult (a, bR) = 
  //   match bR with
  //   | Ok b -> Ok (a, b)
  //   | Error e -> Error e

  // let ofSndOption (a, bO) = 
  //   match bO with
  //   | Some b -> Some (a, b)
  //   | None -> None

  // let ofSndOptionToOption (a , bO) = 
  //   match bO with
  //   | Some b -> Some (a, b)
  //   | None -> None

  // let ofResultsToResults (aR, bR) = 
  //   match (aR, bR) with
  //   | (Ok a, Ok b) ->  Ok (a, b)
  //   | (Error a, _) -> Error a
  //   | (_ , Error b) -> Error b