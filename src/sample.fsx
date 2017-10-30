#r "../packages/FParsec/lib/net40-client/FParsec.dll"
#r "../packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "../packages/Chiron/lib/net45/Chiron.dll"
#r "../packages/Suave/lib/net40/Suave.dll"

#load "CallbackAPI.fs"
#load "SendAPI.fs"
#load "API.fs"



open Pigeon
open Pigeon.CallbackAPI
open Pigeon.SendAPI
open Suave


let send r =
  r |> Request.make "API_TOKEN_GOES_HERE"
  |> Async.RunSynchronously
  |> printfn "%A"

let handler (callback : Callback) =
  for entry in callback.Entries do
    for event in entry.MessagingEvents do
      match event.Message with
      | None -> ()
      | Some m ->
        let action = SenderAction ((Recipient.Id event.Sender), TypingOn)
        send action
        match m.Text with
        | Some text ->
          let response = Message((Recipient.Id event.Sender), Message.Text(sprintf "I got '%s' from you" text, None, None), None)
          send response
        | _ -> ()



let app = Callback.webPart "/webhook" "VERIFICATION_TOKEN_GOES_HERE" handler

open Suave.Logging
let logger = Loggers.ConsoleWindowLogger LogLevel.Verbose

startWebServer {defaultConfig with logger = logger} app


