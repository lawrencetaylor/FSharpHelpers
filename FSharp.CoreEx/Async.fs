module Async.Core

type Async() =

  static member bind f xAsync = 
    async { 
      let! x = xAsync
      let! y = f x
      return y
    }

  static member ret a = async { return a }

  static member map f xAsync = 
    async {
      let! x = xAsync
      return f x
    }

  static member toAsyncSeq x = 
    let rec traverseSeq f list =
      // define the monadic functions
      let (>>=) x f = Async.bind f x
      let retn = Async.ret

      // define a "cons" function
      let cons head tail = [head] |> Seq.ofList |> Seq.append(tail)

      // right fold over the list
      let initState = retn Seq.empty
      let folder head tail = 
          f head >>= (fun h -> 
          tail >>= (fun t ->
          retn (cons h t) ))

      Seq.foldBack folder list initState 
    traverseSeq id x

  static member inline startAsPlainTask (work : Async<unit>) = 
      System.Threading.Tasks.Task.Factory.StartNew(fun () -> work |> Async.RunSynchronously)

  static member sleepForSeconds seconds = 
    Async.Sleep (seconds*1000)