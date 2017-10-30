module Pigeon.SendAPI

open Chiron

let inline private jobj l = JsonObject.ofPropertyList l |> JsonObject.toJson
module E = Chiron.Serialization.Json.Encode
module EI = Chiron.Inference.Json.Encode

type Recipient =
  | Id of string
  | PhoneNumber of string
  with
  static member ToJson(r: Recipient) =
    match r with
    | Id i -> jobj [ "id", String i ]
    | PhoneNumber pn -> jobj [ "phone_number", String pn ]


type AttachmentPayload =
  | Url of url:string * is_reusable:bool
  | Id of string
  with
  static member ToJson(ap: AttachmentPayload) =
    match ap with
    | Url (url, true) -> jobj [ ("url", String url) ; ("is_reusable", True) ]
    | Url (url, false) -> jobj [ "url", String url ]
    | Id i -> jobj [ "attachment_id", String i ]


type Attachment =
  | Image of AttachmentPayload
  | Audio of AttachmentPayload
  | Video of AttachmentPayload
  | File of AttachmentPayload
  with
  static member ToJson (a: Attachment) =
    let t,p =
      match a with
      | Image p -> "image", p
      | Audio p -> "audio", p
      | Video p -> "video", p
      | File p -> "file", p
    JsonObject.empty
    |> EI.required "type" t
    |> EI.required "payload" p
    |> JsonObject.toJson


type QuickReply =
  | Text of title:string * payload:string * image_url:string option
  | Location
  with
  static member ToJson (qr: QuickReply) =
    match qr with
    | Location ->
      jobj ["content_type", String "location"]
    | Text (t, p, i) ->
      JsonObject.empty
      |> EI.required "content_type" "text"
      |> EI.required "title" t
      |> EI.required "payload" p
      |> EI.optional "image_url" i
      |> JsonObject.toJson


type Message =
  | Text of text:string * quick_replies:QuickReply[] option * metadata:string option
  | Attachment of attachment:Attachment * quick_replies:QuickReply[] option * metadata:string option
  with
  static member ToJson (m: Message) =
    match m with
    | Text (t, qrs, md) ->
      JsonObject.empty
      |> EI.required "text" t
      |> EI.optional "quick_replies" qrs
      |> EI.optional "metadata" md
      |> JsonObject.toJson
    | Attachment (a, qrs, md) ->
      JsonObject.empty
      |> EI.required "attachment" a
      |> EI.optional "quick_replies" qrs
      |> EI.optional "metadata" md
      |> JsonObject.toJson


type NotificationType =
  | RegularPush
  | SilentPush
  | NoPush
  with
  static member ToJson (nt: NotificationType) =
    match nt with
    | RegularPush -> String "REGULAR"
    | SilentPush -> String "SILENT_PUSH"
    | NoPush -> String "NO_PUSH"


type SenderAction =
  | MarkAsSeen
  | TypingOn
  | TypingOff
  with
  static member ToJson (sa: SenderAction) =
    match sa with
    | MarkAsSeen -> String "mark_seen"
    | TypingOn -> String "typing_on"
    | TypingOff -> String "typing_off"


type Request =
  | Message of Recipient * Message * NotificationType option
  | SenderAction of Recipient * SenderAction
  with
  static member ToJson (r: Request) =
    match r with
    | Message (r, m, nt) ->
      JsonObject.empty
      |> EI.required "recipient" r
      |> EI.required "message" m
      |> EI.optional "notification_type" nt
      |> JsonObject.toJson
    | SenderAction (r, sa) ->
      JsonObject.empty
      |> EI.required "recipient" r
      |> EI.required "sender_action" sa
      |> JsonObject.toJson