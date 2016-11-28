namespace System.Configuration

module Config =

  let setting (configName : string) (fallback : string) = 
    match box (System.Configuration.ConfigurationManager.AppSettings.[configName]) with
    | null -> fallback
    | config -> config.ToString()

  let connectionString (connectionStringName : string) (fallback : string) = 
    match box (System.Configuration.ConfigurationManager.ConnectionStrings.[connectionStringName]) with
    | null -> fallback
    | config -> config.ToString()