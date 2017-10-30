module Pigeon.CallbackAPI

open Chiron
open Chiron.Operators

module D = Chiron.Serialization.Json.Decode
module DI = Chiron.Inference.Json.Decode

type Attachment =
  | Image of string
  | Audio of string
  | Video of string
  | File of string
  with
  static member FromJson (_:Attachment) = jsonDecoder {
    let! x = DI.required "type"
    let multimediaPayload =
      D.required (D.jsonObject >=> DI.required "url") "payload"
    match x with  // handle location, fallback, and bad types
    | "image" -> return! (multimediaPayload >-> Image)
    | "audio" -> return! (multimediaPayload >-> Audio)
    | "video" -> return! (multimediaPayload >-> Video)
    | "file" -> return! (multimediaPayload >-> File)
  }


type Message = {
  MessageId : string
  Text : string option
  QuickReply : string option
  Attachments : Attachment[] option
} with
  static member FromJson (_:Message) = jsonDecoder {
    let! mid = DI.required "mid"
    let! text = DI.optional "text"
    let! qr = DI.optional "quick_reply"
    let! attachments = DI.optional "attachments"
    return {
      MessageId = mid
      Text = text
      QuickReply = qr
      Attachments = attachments
    }
  }


type MessagingEvent = {
  Sender : string
  Recipient : string
  Message : Message option
} with
  static member FromJson (_:MessagingEvent) = jsonDecoder {
    let! s = D.required (D.jsonObjectWith (D.required D.string "id")) "sender"
    let! r = D.required (D.jsonObjectWith (D.required D.string "id")) "recipient"
    let! m = DI.optional "message"
    return {
      Sender = s
      Recipient = r
      Message = m
    }
  }


type Entry = {
  PageId : string
  Time : int64
  MessagingEvents : MessagingEvent[]
} with
  static member FromJson (_:Entry) = jsonDecoder {
    let! pid = D.required D.string "id"
    let! time = D.required D.int64 "time"
    let! mes = DI.required "messaging"
    return {
      PageId = pid
      Time = time
      MessagingEvents = mes
    }
  }


type Callback = {
  Object : string
  Entries : Entry[]
} with
  static member Parse str : JsonResult<Callback> = Chiron.Inference.Json.deserialize str
  static member FromJson (_:Callback) = jsonDecoder {
    let! o = D.required D.string "object"
    let! es = DI.required "entry"
    return {
      Object = o
      Entries = es
    }
  }