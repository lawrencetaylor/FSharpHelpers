module Json

  open Newtonsoft.Json

  let defaultSettings = JsonSerializerSettings()

  let withConverter converter (settings : JsonSerializerSettings) = 
    settings.Converters.Add(converter)
    settings

  let serialize (settings : JsonSerializerSettings) (x : obj) = JsonConvert.SerializeObject(x, settings)
  
  let deserialize<'a> (settings : JsonSerializerSettings) json = JsonConvert.DeserializeObject<'a>(json, settings)