module Tuple.Core

type Tuple() = 
  static member createWith f a = (a, f a)
  static member diagonal a = Tuple.createWith id a

  static member mapFst f (a, b) = (f a, b)
  static member mapSnd f (a, b) = (a, f b)
  static member mapFstOnSndPredicate p f (a, b) = 
    match p b with
    | true -> Tuple.mapFst f (a, b)
    | false -> (a, b)

  static member fst = fst
  static member snd = snd

  static member both (a, b) =  a && b

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