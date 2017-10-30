namespace Pigeon

module SendAPI =
   open FSharp.Data
   open ParsingHelpers

   let inline private writeStringRecord key value =
      JsonValue.Record [| (key, JsonValue.String value) |]

   type Recipient =
      | Id of string
      | PhoneNumber of string

      static member toJson (r : Recipient) =
         match r with
         | Id id -> writeStringRecord "id" id
         | PhoneNumber pn -> writeStringRecord "phone_number" pn


   type AttachmentPayload =
      | Url of url:string * is_reusable:bool
      | Id of string

      static member toJson (ap : AttachmentPayload) =
         match ap with
         | Url (url, true) ->
            []
            ++ writeString "url" url
            ++ writeBoolean "is_reusable" true
            |> List.toArray
            |> JsonValue.Record
         | Url (url, false) ->
            writeStringRecord "url" url
         | Id id ->
            writeStringRecord "attachment_id" id


   type Attachment =
      | Image of AttachmentPayload
      | Audio of AttachmentPayload
      | Video of AttachmentPayload
      | File of AttachmentPayload

      static member toJson (a : Attachment) =
         let properties =
            match a with
            | Image payload ->
               [] ++ writeString "type" "image" ++ writeJson "payload" (AttachmentPayload.toJson payload)
            | Audio payload ->
               [] ++ writeString "type" "audio" ++ writeJson "payload" (AttachmentPayload.toJson payload)
            | Video payload ->
               [] ++ writeString "type" "video" ++ writeJson "payload" (AttachmentPayload.toJson payload)
            | File payload ->
               [] ++ writeString "type" "file" ++ writeJson "payload" (AttachmentPayload.toJson payload)
         properties
         |> List.toArray
         |> JsonValue.Record


   type QuickReply =
      | Text of title:string * payload:string * image_url:string option
      | Location

      static member toJson (qr : QuickReply) =
         match qr with
         | Location ->
            writeStringRecord "content_type" "location"
         | Text (title, payload, image_url) ->
            []
            ++ writeString "content_type" "text"
            ++ writeString "title" title
            ++ writeString "payload" payload
            +? opt (writeString "image_url") image_url
            |> List.toArray
            |> JsonValue.Record

   type Message =
      | Text of text:string * quick_replies:QuickReply[] option * metadata:string option
      | Attachment of attachment:Attachment * quick_replies:QuickReply[] option * metadata:string option

      static member toJson (m : Message) =
         let properties =
            match m with
            | Text (text, quick_replies, metadata) ->
               []
               ++ writeString "text" text
               +? opt (writeArray "quick_replies" QuickReply.toJson) quick_replies
               +? opt (writeString "metadata") metadata
            | Attachment (attachment, quick_replies, metadata) ->
               []
               ++ writeJson "attachment" (Attachment.toJson attachment)
               +? opt (writeArray "quick_replies" QuickReply.toJson) quick_replies
               +? opt (writeString "metadata") metadata
         properties
         |> List.toArray
         |> JsonValue.Record




   type NotificationType =
      | RegularPush
      | SilentPush
      | NoPush

      static member toJson (n : NotificationType) =
         match n with
         | RegularPush -> JsonValue.String "REGULAR"
         | SilentPush -> JsonValue.String "SILENT_PUSH"
         | NoPush -> JsonValue.String "NO_PUSH"


   type SenderAction =
      | MarkAsSeen
      | TypingOn
      | TypingOff

      static member toJson (sa : SenderAction) =
         match sa with
         | MarkAsSeen -> JsonValue.String "mark_seen"
         | TypingOn -> JsonValue.String "typing_on"
         | TypingOff -> JsonValue.String "typing_off"


   type Request =
      | Message of Recipient * Message * NotificationType option
      | SenderAction of Recipient * SenderAction

      static member toJson (r : Request) =
         match r with
         | Message (recipient, message, notification_type) ->
            []
            ++ writeJson "recipient" (Recipient.toJson recipient)
            ++ writeJson "message" (Message.toJson message)
            +? opt (writeJson "notification_type") (opt NotificationType.toJson notification_type)
            |> List.toArray
            |> JsonValue.Record
         | SenderAction (recipient, sender_action) ->
            []
            ++ writeJson "recipient" (Recipient.toJson recipient)
            ++ writeJson "sender_action" (SenderAction.toJson sender_action)
            |> List.toArray
            |> JsonValue.Record
