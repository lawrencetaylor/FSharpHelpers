[<AutoOpen>]
module Option.Async

open Async
open Option.Core

type Option() =

  static member ofAsyncToAsync = Option.map(Async.map(Some)) >> Option.fallback (Async.ret None)