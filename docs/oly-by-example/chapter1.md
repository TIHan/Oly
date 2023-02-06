**Note: This is a work-in-progress.**

# Chapter 1 - `Hello World!`
## `Hello World!` (.NET 7)

file: [`chapter1/hello-world-net7.olyx`](chapter1/hello-world-net7.olyx)
```oly
#target "dotnet: net7"

open System

main(): () =
    Console.WriteLine("Hello World!")
```