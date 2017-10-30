namespace Pigeon

[<RequireQualifiedAccess>]
module Request =
  open System
  open System.Net.Http
  open System.Net.Http.Headers
  open Chiron.Inference
  open SendAPI

  let private appendQueryToUrl query (url: string) =
    match query with
    | [] -> url
    | query ->
      url
      + if url.Contains "?" then "&" else "?"
      + String.concat "&" [ for k, v in query -> Uri.EscapeDataString k + "=" + Uri.EscapeDataString v ]

  let private client = new HttpClient() // TODO: properly implement singleton

  let make accessToken (r : Request) =
    let uri =
      "https://graph.facebook.com/v2.6/me/messages"
      |> appendQueryToUrl [ "access_token", accessToken ]
      |> Uri
    let body = new StringContent(Json.serialize r, Text.Encoding.UTF8, "application/json")
    client.PostAsync(uri, body) |> Async.AwaitTask



[<RequireQualifiedAccess>]
module Callback =
  open Suave
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful

  open Chiron
  open Pigeon.CallbackAPI

  let webPart urlPath verifyToken handler =
    path urlPath >=> choose[
      // Page subscription verification is sent via GET requests
      GET >=>
        request (fun r ->
        match r.queryParam("hub.mode"), r.queryParam("hub.verify_token"), r.queryParam("hub.challenge") with
        | (Choice1Of2 "subscribe"), (Choice1Of2 vt), (Choice1Of2 challenge) when vt = verifyToken ->
           OK challenge
        | _ ->
           OK "Something went wrong"
        )

      // Callbacks are sent via POST requests
      POST >=>
        request (fun r ->
        let data = System.Text.Encoding.UTF8.GetString(r.rawForm)
        match Callback.Parse data with
        | JPass cb -> handler cb
        | JFail _ -> ()
        ok [||])
    ]


  let parse data =
    Callback.Parse data |> JsonResult.getOrThrow