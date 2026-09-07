**Note: This is a work-in-progress.**

# Chapter 1 - `Hello World!`
## `Hello World!` (.NET 10)

file: [`chapter1/hello-world-net10.olyx`](chapter1/hello-world-net10.olyx)
```oly
#target "dotnet: net10"

open System

main(): () =
    Console.WriteLine("Hello World!")
```