namespace Pigeon

module ParsingHelpers =
   open FSharp.Data

   type ParseError =
      | MissingField of string
      | WrongFieldType of string
      | UnexpectedValueForField of string

   type ParseResult<'a> =
      | Success of 'a
      | Error of ParseError

   type FieldRequirement =
      | Required
      | Optional

   let bind (f : 'a -> ParseResult<'b>) (m : ParseResult<'a>) =
      match m with
      | Success a -> f a
      | Error e -> Error e

   let inline (>>=) m f =
      bind f m



   let readField (json : JsonValue) key =
      match json.TryGetProperty(key) with
      | Some value -> Success value
      | None -> Error (MissingField key)



   let readString json key (compose : string -> 'a) =
      match readField json key with
      | Success (JsonValue.String value) -> Success (compose value)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e

   let readOptionalString json key (compose : string option -> 'a) =
      match readField json key with
      | Success (JsonValue.String value) -> Success (compose (Some value))
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e



   let readDecimal json key (compose : decimal -> 'a) =
      match readField json key with
      | Success (JsonValue.Number value) -> Success (compose value)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e

   let readOptionalDecimal json key (compose : decimal option -> 'a) =
      match readField json key with
      | Success (JsonValue.Number value) -> Success (compose (Some value))
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e


   let readFloat json key (compose : float -> 'a) =
      match readField json key with
      | Success (JsonValue.Float value) -> Success (compose value)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e

   let readOptionalFloat json key (compose : float option -> 'a) =
      match readField json key with
      | Success (JsonValue.Float value) -> Success (compose (Some value))
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e


   let readBoolean json key (compose : bool -> 'a) =
      match readField json key with
      | Success (JsonValue.Boolean value) -> Success (compose value)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e

   let readOptionalBoolean json key (compose : bool option -> 'a) =
      match readField json key with
      | Success (JsonValue.Boolean value) -> Success (compose (Some value))
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e



   let readArray json key (parse : JsonValue -> ParseResult<'a>) (compose : 'a[] -> 'b) =
      match readField json key with
      | Success (JsonValue.Array values) ->
         let result =
            List.foldBack (
               fun j accIn ->
                  match accIn with
                  | Success list ->
                     match parse j with
                     | Success a -> Success (a :: list)
                     | Error e -> Error e
                  | Error e -> Error e
            ) (List.ofArray <| values) (Success [])
         match result with
         | Success list -> Success (compose (Array.ofList list))
         | Error e -> Error e
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e


   let readOptionalArray json key (parse : JsonValue -> ParseResult<'a>) (compose : 'a[] option -> 'b) =
      match readField json key with
      | Success (JsonValue.Array values) ->
         let result =
            List.foldBack (
               fun j accIn ->
                  match accIn with
                  | Success list ->
                     match parse j with
                     | Success a -> Success (a :: list)
                     | Error e -> Error e
                  | Error e -> Error e
            ) (List.ofArray <| values) (Success [])
         match result with
         | Success list -> Success (compose (Some (Array.ofList list)))
         | Error e -> Error e
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e


   let readRecord json key (parse : JsonValue -> ParseResult<'a>) (compose : 'a -> 'b) =
      match readField json key with
      | Success ((JsonValue.Record _) as record) ->
         match parse record with
         | Success a -> Success (compose a)
         | Error e -> Error e
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e

   let readOptionalRecord json key (parse : JsonValue -> ParseResult<'a>) (compose : 'a option -> 'b) =
      match readField json key with
      | Success ((JsonValue.Record _) as record) ->
         match parse record with
         | Success a -> Success (compose (Some a))
         | Error e -> Error e
      | Error (MissingField _) -> Success (compose None)
      | Success _ -> Error (WrongFieldType key)
      | Error e -> Error e


   let inline (++) accIn pair =
      pair :: accIn


   let writeOpt (pair : (string * JsonValue) option) accIn =
      match pair with
      | Some p -> p :: accIn
      | None -> accIn

   let inline (+?) accIn pair =
      writeOpt pair accIn


   let opt (f : 'a -> 'b) (a : 'a option) =
      match a with
      | Some v -> Some (f v)
      | None -> None


   let inline writeString (key:string) value =
      (key, JsonValue.String value)

   let inline writeDecimal (key:string) value =
      (key, JsonValue.Number value)

   let inline writeFloat (key:string) value =
      (key, JsonValue.Float value)

   let inline writeBoolean (key:string) value =
      (key, JsonValue.Boolean value)

   let inline writeArray (key:string) (parse : 'a -> JsonValue) values =
      (key, JsonValue.Array <| Array.map parse values)

   let inline writeJson (key:string) (value : JsonValue) =
      (key, value)