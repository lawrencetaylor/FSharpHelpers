module Option.Async

open Async.Core
open Option.Core

type Option() =

  static member ofAsyncToAsync oA = 
    oA |> (Option.map(Async.map(Some)) >> Option.fallback (Async.ret None))

type Async() = 

  // I dont' like the Async.RunSynchronously call here.. Need to work out how to get rid of this! 
  static member ofOptionToOption aO = 
    aO |> (Async.map(Option.map(fun a -> a |> Async.ret)) >> Async.RunSynchronously)

    // let b = Async.bind

    // let inner aO = 
    //   async {
    //     let! o = aO
    //     return
    //       match o with
    //       | Some x -> Some (async { return x })
    //       | None -> None
    //   } |> Async.RunSynchronously
    // inner 