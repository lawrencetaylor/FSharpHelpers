module Retry

  open Option
  open Result

  type AttemptResult<'a> =
    | Success of 'a
    | Failure of string

  let private isSuccess ar =
    match ar with
    | Success _ -> true
    | _ -> false

  type private RetryState<'a> =  AttemptResult<'a> option

  let private execute f =
    try
     Success (f ())
    with
    | ex -> ex |> sprintf "ERROR: %A" |> Failure

  let private tryAgain currentState f = Some (execute f)

  let private unwrapRetry interval f retryState =
    match retryState with
    | None -> Some (None, tryAgain retryState f)
    | Some (Success a) ->
      Some (Some (Success a), retryState)
    | Some (Failure s) ->
      Async.sleepForSeconds interval |> Async.RunSynchronously
      Some ((Some (Failure s)), tryAgain retryState f)


  let retry (f : unit -> 'a) limit interval =
    Seq.unfold (unwrapRetry interval f) None
    |> Seq.take limit
    |> Seq.tryFind (
      fun x ->
        match x with
        | Some (Success _) -> true
        | _ -> false)
    |> Option.bind id
    |> Option.fallback (Failure "Method was never attempted")

  // let throwOnUnhandled (r : Result.T<_, CommandErrors>) =
  //   match r with
  //   | Result.Error (CommandErrors.Unhandled msg) -> failwith msg
  //   | _ -> r