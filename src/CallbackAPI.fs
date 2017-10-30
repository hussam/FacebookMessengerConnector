namespace Pigeon

module CallbackAPI =
   open FSharp.Data
   open ParsingHelpers


   type Attachment =
      | Image of string
      | Audio of string
      | Video of string
      | File of string
      | Coordinates of lat:decimal * long:decimal

   type Message = {
      mid : string
      seq : int
      text : string option
      quick_reply : string option
      attachments : Attachment[] option
   }

   type MessagingEvent = {
      sender : string
      recipient : string
      timestamp : int64
      message : Message option
      postback : string option
      optin : string option
   }

   type Entry = {
      id : string
      time : int64
      messagingEvents : MessagingEvent[]
   }

   type Callback = {
      Object : string
      Entries : Entry[]
   }


   let private readStringRecord key json =
      (Success (fun v -> v))
      >>= readString json key

   let inline private readPayload json =
      readStringRecord "payload" json


   let private readId json =
      (Success (fun i -> i))
      >>= readString json "id"



   type Attachment with
      static member fromJson (json : JsonValue) =
         let urlPayloadFromJson (json : JsonValue) =
            let f url = url
            (Success f)
            >>= readString json "url"

         let coordinatesPayloadFromJson (json : JsonValue) =
            let f lat long = (lat, long)
            (Success f)
            >>= readDecimal json "coordinates.lat"
            >>= readDecimal json "coordinates.long"

         match readString json "type"(fun x -> x) with
         | Success "image" ->  readRecord json "payload" urlPayloadFromJson (fun url -> Image url)
         | Success "audio" ->  readRecord json "payload" urlPayloadFromJson (fun url -> Audio url)
         | Success "video" ->  readRecord json "payload" urlPayloadFromJson (fun url -> Video url)
         | Success "file" ->   readRecord json "payload" urlPayloadFromJson (fun url -> File url)
         | Success "coordinates" ->  readRecord json "payload" coordinatesPayloadFromJson (fun coordinates -> Coordinates coordinates)
         | Success _ -> Error (UnexpectedValueForField "type")
         | Error e -> Error e


   type Message with
      static member fromJson (json : JsonValue) =
         let f mid seq text quick_reply attachments =
            { mid = mid; seq = int seq; text = text; quick_reply = quick_reply; attachments = attachments }

         (Success f)
         >>= readString json "mid"
         >>= readDecimal json "seq"
         >>= readOptionalString json "text"
         >>= readOptionalRecord json "quick_reply" readPayload
         >>= readOptionalArray json "attachment" Attachment.fromJson


   type MessagingEvent with
      static member fromJson (json : JsonValue) =
         let f sender recipient timestamp message postback optin =
            { sender = sender ; recipient = recipient; timestamp = (int64 timestamp); message = message; postback = postback; optin = optin }

         (Success f)
         >>= readRecord json "sender" readId
         >>= readRecord json "recipient" readId
         >>= readDecimal json "timestamp"
         >>= readOptionalRecord json "message" Message.fromJson
         >>= readOptionalRecord json "postback" readPayload
         >>= readOptionalRecord json "optin" (readStringRecord "ref")


   type Entry with
      static member fromJson (json : JsonValue) =
         let f id time messagingEvents =
            {id = id; time = (int64 time); messagingEvents = messagingEvents}

         (Success f)
         >>= readString json "id"
         >>= readDecimal json "time"
         >>= readArray json "messaging" MessagingEvent.fromJson


   type Callback with
      static member fromJson (json : JsonValue) =
         let f o entries =
            {Object = o; Entries = entries}

         (Success f)
         >>= readString json "object"
         >>= readArray json "entry" Entry.fromJson
