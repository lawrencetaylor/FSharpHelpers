module Tuple.Core

type Tuple() = 

  let createW f a = (a, f a)

  static member CreateWith f a = (a, f a)
  static member Diagonal a = Tuple.CreateWith id a

  static member MapFst f (a, b) = (f a, b)
  static member MapSnd f (a, b) = (a, f b)
  static member MapFstOnSndPredicate p f (a, b) = 
    match p b with
    | true -> Tuple.MapFst f (a, b)
    | false -> (a, b)

  static member Fst = fst
  static member Snd = snd

  static member Both (a, b) =  a && b

  // let ofSndResult (a, bR) = 
  //   match bR with
  //   | Ok b -> Ok (a, b)
  //   | Error e -> Error e

  // let ofSndOption (a, bO) = 
  //   match bO with
  //   | Some b -> Some (a, b)
  //   | None -> None



  // let ofResultsToResults (aR, bR) = 
  //   match (aR, bR) with
  //   | (Ok a, Ok b) ->  Ok (a, b)
  //   | (Error a, _) -> Error a
  //   | (_ , Error b) -> Error b