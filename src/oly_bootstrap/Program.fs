open olylib
open Oly.Core

let results = Oly.Build("Release", OlyPath.Create("../../../oly/oly.olyx"), System.Threading.CancellationToken.None).Result
if results.IsError then
    failwith "Failed: Oly Cli Bootstrap"
else
    printfn "Oly Cli Bootstrap Succeeded"