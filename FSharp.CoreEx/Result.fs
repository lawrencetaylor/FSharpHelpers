namespace FSharp.CoreEx

type Result<'a, 'b> = 
  Ok of 'a
  | Error of 'b

module Result = 

  let ofOption error option = 
    match option with
    | None -> Error error
    | Some o -> Ok o

  let throwIfError result = 
    match result with
    | Ok a -> a
    | Error error -> failwithf "Result.Error: %A" error

  let ret a = Ok a

  let tryGetOk result = 
    match result with
    | Ok ok -> Some ok
    | Error _ -> None

  let ok (r : Result<'a, _>) = (tryGetOk >> Option.get) r

  let apply (resultF : Result<'a ->'b, _>) (resultA : Result<'a, _>) = 
    match resultF, resultA with
    | Ok f, Ok a -> Ok (f a)
    | Error fError, _ -> Error fError
    | _ , Error aError -> Error aError
  let map (fOk, fError) result = 
    match result with
    | Ok a  -> Ok (fOk a)
    | Error error -> Error(fError error)

  let mapOk f = map (f, id)
  let mapError f = map (id, f)

  let bindOk (f : 'a -> Result<'b, 'c>) aR = 
    match aR with
    | Ok a -> a |> f
    | Error x -> Error x

  let ofAsyncToAsync xRA = 
    match xRA with
    | Ok xA -> 
      async { 
        let! x = xA 
        return Ok x}
    | Error e -> async { return Error e}