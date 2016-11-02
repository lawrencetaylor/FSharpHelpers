namespace FSharp.CoreEx

module Json = 

  let serialize (x : obj) = Newtonsoft.Json.JsonConvert.SerializeObject(x)
  
  let deserialize<'a> json = Newtonsoft.Json.JsonConvert.DeserializeObject<'a>(json)