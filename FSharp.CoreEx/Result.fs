module Result.Core

  type Result<'a, 'b> = 
    Ok of 'a
    | Error of 'b

  type Result() =
    static member ofOption error option = 
      match option with
      | None -> Error error
      | Some o -> Ok o

    static member throwIfError result = 
      match result with
      | Ok a -> a
      | Error error -> failwithf "Result.Error: %A" error

    static member ret a = Ok a

    static member tryGetOk result = 
      match result with
      | Ok ok -> Some ok
      | Error _ -> None

    static member ok (r : Result<'a, _>) = (Result.tryGetOk >> Option.get) r

    static member apply (resultF : Result<'a ->'b, _>) (resultA : Result<'a, _>) = 
      match resultF, resultA with
      | Ok f, Ok a -> Ok (f a)
      | Error fError, _ -> Error fError
      | _ , Error aError -> Error aError

    static member map (fOk, fError) result = 
      match result with
      | Ok a  -> Ok (fOk a)
      | Error error -> Error(fError error)

    static member mapOk f = Result.map (f, id)
    static member mapError f = Result.map (id, f)

    static member bindOk (f : 'a -> Result<'b, 'c>) aR = 
      match aR with
      | Ok a -> a |> f
      | Error x -> Error x

    static member ofAsyncToAsync xRA = 
      match xRA with
      | Ok xA -> 
        async { 
          let! x = xA 
          return Ok x}
      | Error e -> async { return Error e}