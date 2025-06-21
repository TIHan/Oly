module Oly.Core.Json

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Text.Json.Serialization.Metadata

let Deserialize<'T>(utf8JsonStream: Stream) =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNameCaseInsensitive <- true
    //jsonOptions.TypeInfoResolver <-
    //    { new Metadata.IJsonTypeInfoResolver with
    //            member this.GetTypeInfo(``type``: Type, options: JsonSerializerOptions): Metadata.JsonTypeInfo = 
    //                raise (System.NotImplementedException(``type``.ToString())) }
    JsonSerializer.Deserialize<'T>(utf8JsonStream, jsonOptions)

