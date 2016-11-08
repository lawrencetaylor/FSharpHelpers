module Tuple.Option

open Async.Core
open Result.Core

type Tuple() = 
  static member ofSndOptionToOption (a , bO) = 
    match bO with
    | Some b -> Some (a, b)
    | None -> None

  static member ofSndAsyncResultToAsyncResult (a, bAR) : Async<Result<'a*'b, _>> = 
    async {
      let! bR = bAR
      return 
        match bR with
        | Result.Ok b -> Result.Ok (a, b)
        | Result.Error e -> Result.Error e
    }
