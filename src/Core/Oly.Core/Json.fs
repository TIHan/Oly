module Oly.Core.Json

open System.IO
open System.Text.Json
open System.Threading
open Oly.Core

let Serialize<'T>(utf8JsonStream: Stream, value: 'T) =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNameCaseInsensitive <- true
    jsonOptions.WriteIndented <- true
    JsonSerializer.Serialize<'T>(utf8JsonStream, value, jsonOptions)

let SerializAsync<'T>(utf8JsonStream: Stream, value: 'T, ct: CancellationToken) =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNameCaseInsensitive <- true
    jsonOptions.WriteIndented <- true
    JsonSerializer.SerializeAsync<'T>(utf8JsonStream, value, jsonOptions, ct)

let Deserialize<'T>(utf8JsonStream: Stream) =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNameCaseInsensitive <- true
    JsonSerializer.Deserialize<'T>(utf8JsonStream, jsonOptions)

let DeserializeAsync<'T>(utf8JsonStream: Stream, ct: CancellationToken) =
    let jsonOptions = JsonSerializerOptions()
    jsonOptions.PropertyNameCaseInsensitive <- true
    JsonSerializer.DeserializeAsync<'T>(utf8JsonStream, jsonOptions, ct)

let SerializeAsFileAsync<'T>(filePath: OlyPath, value: 'T, ct: CancellationToken) =
    backgroundTask {
        use fs = File.Open(filePath.ToString(), FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite)
        do! SerializAsync<'T>(fs, value, ct)
    }

let DeserializeFromFileAsync<'T>(filePath: OlyPath, ct: CancellationToken) =
    backgroundTask {
        use fs = File.Open(filePath.ToString(), FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.ReadWrite)
        return! DeserializeAsync<'T>(fs, ct)
    }

