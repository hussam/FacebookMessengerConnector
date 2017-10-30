namespace Pigeon

module Extras =

   module CallbackAPISerialization =
      open FSharp.Data
      open CallbackAPI
      open ParsingHelpers


      let private writeOptionalRecord key value =
         match value with
         | Some v -> Some (JsonValue.Record [| key, JsonValue.String v |])
         | None -> None

      let inline private writeOptionalPayload value =
         writeOptionalRecord "payload" value


      type Attachment with
         static member toJson (a : Attachment) =
            let type', payload =
               match a with
               | Image url -> "image", [| ("url", JsonValue.String url) |]
               | Audio url -> "audio", [| ("url", JsonValue.String url) |]
               | Video url -> "video", [| ("url", JsonValue.String url) |]
               | File url -> "file", [| ("url", JsonValue.String url) |]
               | Coordinates (lat, long) -> "coordinates", [| ("coordinates.lat", JsonValue.Number lat) ; ("coordinates.long", JsonValue.Number long) |]
            [| ("type", JsonValue.String type') ; ("payload", JsonValue.Record payload) |]
            |> JsonValue.Record


      type Message with
         static member toJson (m : Message) =
            []
            ++ writeString "mid" m.mid
            ++ writeDecimal "seq" (decimal m.seq)
            +? opt (writeString "text") m.text
            +? opt (writeJson "quick_reply") (writeOptionalPayload m.quick_reply)
            +? opt (writeArray "attachment" Attachment.toJson) m.attachments
            |> List.toArray
            |> JsonValue.Record


      type MessagingEvent with
         static member toJson (e : MessagingEvent) =
            let id i = JsonValue.Record [| ("id", JsonValue.String i) |]
            let message = match e.message with | Some m -> Some (Message.toJson m) | None -> None

            []
            ++ writeJson "sender" (id e.sender)
            ++ writeJson "recipient" (id e.recipient)
            ++ writeDecimal "timestamp" (decimal e.timestamp)
            +? opt (writeJson "message") message
            +? opt (writeJson "postback") (writeOptionalPayload e.postback)
            +? opt (writeJson "optin") (writeOptionalRecord "ref" e.optin)
            |> List.toArray
            |> JsonValue.Record

      type Entry with
         static member toJson (e : Entry) =
            []
            ++ writeString "id" e.id
            ++ writeDecimal "time" (decimal e.time)
            ++ writeArray "messaging" MessagingEvent.toJson e.messagingEvents
            |> List.toArray
            |> JsonValue.Record

      type Callback with
         static member toJson (c : Callback) =
            []
            ++ writeString "object" c.Object
            ++ writeArray "entry" Entry.toJson c.Entries
            |> List.toArray
            |> JsonValue.Record
