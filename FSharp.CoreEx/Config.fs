namespace FSharp.CoreEx

module Config = 

  let get (configName : string) (fallback : string) = 
    match box (System.Configuration.ConfigurationManager.AppSettings.[configName]) with
    | null -> fallback
    | config -> config.ToString()