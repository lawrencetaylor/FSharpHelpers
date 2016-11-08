module MassTransitMessaging

open Async.Core
open Config.Core


/// This attribute is used to demark a module that is responsible for handling with asynchronous messages.  When applied the modules `handle` method is automatically registered with the mesage bus to route messags of the relevant type to this module.
/// The `QueueName` is the queue that the handler is listening to messages on.
/// The convention is that the module with this attribute has a field named `handle`, which is either of type
/// * `MessageBus.Subscription<'a>` or
/// * `IBusControl` -> `MessageBus.Subscription<'a>`
/// Where `'a` is the type of the message being handled
[<System.AttributeUsage(System.AttributeTargets.Class, AllowMultiple = false)>]
type MessageHandlerAttribute(queueName : string) =
  inherit System.Attribute()
  member val QueueName = queueName with get

  module MessageHandler = 

  let queueName (messageHandler : MessageHandlerAttribute ) = messageHandler.QueueName


/// This attribute is used to demark a module that is responsible for dealing with asynchronous request-response scenarios.  When applied the modules `handle` method is automatically registered with the mesage bus to route messags of the relevant type to this module.
/// The `QueueName` is the queue that the handler is listening to messages on.
/// The convention is that the module with this attribute has a field named `handle` of type  `MessageBus.Subscription<'a>` where 'a is the request message
/// For a module with this attribute, a client factory is generated on start up, which allows you to generate a client from an instance of `IBusControl` using
/// ```
///   open Infrastructure.MessageBus
///   let requestClient = bus.CreateRequestClient<'a,'b>(timeOut)
/// ```
[<System.AttributeUsage(System.AttributeTargets.Class, AllowMultiple = false)>]
type RequestHandlerAttribute(queueName : string) =
  inherit System.Attribute()

  member val QueueName = queueName with get

module RequestHandler = 

  let queueName (messageHandler : RequestHandlerAttribute ) = messageHandler.QueueName



module MessageBus = 

  open System
  open MassTransit
  open MassTransit.RabbitMqTransport
  open MassTransit.Pipeline 

  let mutable busRequestQueues  = Map.empty

  type MassTransit.IBusControl with

    member x.RegisterRequest (t : System.Type) (queueName) = 
      let thisBus = x.Address.AbsolutePath
      busRequestQueues <-
        busRequestQueues 
        |> Map.add 
          thisBus
          (match busRequestQueues |> Map.tryFind thisBus with
          | Some queueMap -> queueMap |> Map.add (t.ToString()) queueName
          | Option.None -> [t.ToString(), queueName ] |> Map.ofList)


    member x.CreateRequestClient<'a, 'b when 'a : not struct and 'b : not struct> timeSpan = 
      let thisBusQueues = busRequestQueues.[x.Address.AbsolutePath]
      let thisRequest = typeof<'a>.ToString()
      let (queueName : string ) = thisBusQueues.[thisRequest]
      new MassTransit.MessageRequestClient<'a, 'b>(x, new System.Uri(queueName), timeSpan)

  type RetryPolicy = 
    None
    | Exponential
    | Interval
    member internal x.ToMassTransitPolicy = 
      match x with
      | None -> MassTransit.Retry.None
      | Exponential -> MassTransit.Retry.Exponential (TimeSpan.FromSeconds(2.0), TimeSpan.FromSeconds(10.0), TimeSpan.FromSeconds(3.0))
      | Interval  -> failwith "Haven't thought about this yet"

  type ConsumerConfig internal (queueName, t : Type, factory : IBusControl -> Type -> obj, retryPolicy : RetryPolicy)  = 
    member internal this.Type = t
    member internal this.Factory =  factory
    member internal this.QueueName = queueName
    member internal this.Retry = retryPolicy

  type MessageBusConfig =
    { 
      Host : string
      Username : string
      Password : string

      MessageConsumers : ConsumerConfig list
      RequestQueues : (Type* string) list
      RecieveObservers : IReceiveObserver list
    }

  let defaultConfig = 
    { Host = "boot2docker";  
      Username = "guest"; 
      Password = "guest";
      MessageConsumers = []
      RequestQueues = [] 
      RecieveObservers = []}

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module MessageBusConfig = 

    let addConsumers consumers config = { config with MessageConsumers = config.MessageConsumers |> List.append consumers}

    let addRequestQueues requestQueues config = { config with RequestQueues = config.RequestQueues |> List.append requestQueues}

    let addReceiveObservers receiveObservers config = { config with RecieveObservers = config.RecieveObservers |> List.append receiveObservers}


  type Subscription<'a when 'a : not struct> = ConsumeContext<'a> -> Async<unit>

  let broadcast<'a when 'a : not struct> (bus : MassTransit.IBusControl) (headers : (string*obj) list) (msg : 'a) = 
    let x : IPipe<PublishContext<'a>> = 
      Pipe.New<PublishContext<'a>>(
        fun c -> 
          c.UseExecute(fun a -> 
            headers |> List.iter(fun (k, v) -> a.Headers.Set(k, v))))

    bus.Publish(msg :> obj, x, System.Threading.CancellationToken.None) |> Async.AwaitTask

  module Consumer = 

    let tryGetHeader headerKey (context : ConsumeContext<_>) =
      match context.Headers.TryGetHeader headerKey with
      | (true, connectionId) -> (connectionId :?> string) |> Some
      | (false, _ ) -> Option.None

    let broadcastWithHeaders<'a, 'b when 'a :  not struct and 'b : not struct>
        headers (context : ConsumeContext<'a>) (msg : 'b)  = 

      let headers = 
        context.Headers.GetAll() 
        |> Seq.map(fun h -> (h.Key, h.Value))
        |> Seq.append(headers)

      let x : MassTransit.Pipeline.IPipe<PublishContext<'b>> = 
        Pipe.New<PublishContext<'b>>(
          fun c -> 
            c.UseExecute(fun a -> 
              headers |> Seq.iter(fun (k, v) -> a.Headers.Set(k, v))))
      context.Publish<'b>(msg, x) |> Async.AwaitTask

    let broadcast<'a, 'b when 'a :  not struct and 'b : not struct> = broadcastWithHeaders<'a, 'b> []

    let respond<'a, 'b when 'a :  not struct and 'b : not struct>
        (context : ConsumeContext<'a>) (msg : 'b) = 

      let headers = context.Headers.GetAll() |> Seq.map(fun h -> (h.Key, h.Value))

      let x : MassTransit.Pipeline.IPipe<SendContext<'b>> = 
        Pipe.New<SendContext<'b>>(
          fun c -> 
            c.UseExecute(fun a -> 
              headers |> Seq.iter(fun (k, v) -> a.Headers.Set(k, v))))
      context.RespondAsync<'b>(msg, x) |> Async.AwaitTask

  let private registerConsumer 
    (host : IRabbitMqHost) 
    (config : IRabbitMqBusFactoryConfigurator) 
    (busControl : Ref<IBusControl>)
    (consumerConfig : ConsumerConfig)
     =
    let handle = [|busControl|]
    config.ReceiveEndpoint(host, consumerConfig.QueueName, fun e -> 
      e.Consumer(consumerConfig.Type, fun t ->  consumerConfig.Factory !handle.[0] t)
      e.ConfigurePublish(
        fun a -> 
          a.UseSendExecute(
            fun (e : SendContext) -> 
              e.Headers.Set("A", "B")
              ()))
      e.UseRetry consumerConfig.Retry.ToMassTransitPolicy)

  let createWithConfig busConfig = 
    let config = 
      { busConfig with
          Host =      Config.get "Messaging::Host" defaultConfig.Host
          Username =  Config.get "Messaging::Username" defaultConfig.Username
          Password =  Config.get "Messaging::Password" defaultConfig.Password
      }
    let uri = System.Uri(sprintf "rabbitmq://%s" config.Host)
    let busRef = ref null
    busRef :=
        (Bus.Factory.CreateUsingRabbitMq(
          fun x -> 
            let host = 
              x.Host(uri, 
                fun h -> 
                  h.Username config.Username
                  h.Password config.Password
            )
            config.MessageConsumers
            |> List.iter(fun y -> registerConsumer host x busRef y)))

    let bus = !busRef       
    config.RequestQueues |> List.iter(fun (t,q) -> bus.RegisterRequest t (sprintf "rabbitmq://%s/%s" config.Host q))
    config.RecieveObservers |> List.iter(fun o -> bus.ConnectReceiveObserver(o) |> ignore)
    (bus, bus.Start())

  type private MessageBusConsumer<'a when 'a : not struct>(f : Subscription<'a>) = 

    interface IConsumer<'a> with
      member x.Consume context = context |> f |> Async.startAsPlainTask

  type private MessageBusEventRaisingConsumer<'a when 'a : not struct>(observable :IObserver<'a>) = 

    interface IConsumer<'a> with
      member x.Consume context = 
        async { return observable.OnNext(context.Message) }
        |> Async.startAsPlainTask

  let createConsumer<'a when 'a : not struct> queueName (f : IBusControl -> Subscription<'a>) retryPolicy = 
    ConsumerConfig(queueName, typeof<MessageBusConsumer<'a>>, (fun bus _ -> MessageBusConsumer<'a>(f bus) :> obj), retryPolicy)

  let createObservableConsumer<'a when 'a : not struct> queueName (o : IObserver<'a>) retryPolicy=
    ConsumerConfig(queueName, typeof<MessageBusEventRaisingConsumer<'a>>, (fun _ _ -> MessageBusEventRaisingConsumer<'a>(o) :> obj), retryPolicy)


  
   

   
    