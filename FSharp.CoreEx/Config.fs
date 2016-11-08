[<AutoOpen>]
module Config.Core

type Config() =

  static member get (configName : string) (fallback : string) = 
    match box (System.Configuration.ConfigurationManager.AppSettings.[configName]) with
    | null -> fallback
    | config -> config.ToString()