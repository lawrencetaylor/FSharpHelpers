[<AutoOpen>]
module Json.Core

open Newtonsoft.Json

type Json () = 

  static member defaultSettings = JsonSerializerSettings()

  static member withConverter converter (settings : JsonSerializerSettings) = 
    settings.Converters.Add(converter)
    settings

  static member serialize (settings : JsonSerializerSettings) (x : obj) = JsonConvert.SerializeObject(x, settings)
  
  static member deserialize<'a> (settings : JsonSerializerSettings) json = JsonConvert.DeserializeObject<'a>(json, settings)