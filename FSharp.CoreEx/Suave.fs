
  
  module HttpRequest

    open Suave
    open Suave.Successful
    open Json.Core

    let deserialize<'a> settings = Json.deserialize<'a> settings

    let serialize settings : (obj -> string) = Json.serialize settings

    open Microsoft.FSharp.Reflection
    let private requestBody<'a> settings request = 
      request.rawForm
      |> System.Text.Encoding.UTF8.GetString
      |> deserialize<'a> settings

    let private makeRequest<'a, 'b> settings (f : 'a -> Async<'b>) (onResult : 'b -> WebPart)  = 
      fun (ctx : HttpContext) -> 
        async {
          let req = requestBody<'a> settings ctx.request
          let! res = f req
          return! onResult res ctx
        }

    let private makeRequestSimple<'a, 'b> settings (f : 'a -> Async<'b>) : WebPart = makeRequest settings f (serialize settings >> OK)

    let fromFunction settings (f : 'a -> Async<'b>) (onResult : 'b -> WebPart) = makeRequest settings f onResult

    let fromFunctionSimple settings (f : 'a -> Async<'b>) = makeRequestSimple settings f

