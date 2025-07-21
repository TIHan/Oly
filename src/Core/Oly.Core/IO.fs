namespace Oly.Core.IO

open System
open System.IO
open Oly.Core

/// Skips files with the '__oly' prefix.
[<RequireQualifiedAccess; Sealed; AbstractClass>]
type OlyIO private () =

    static member private CopyFileToDirectory(file: FileInfo, dstDir) =
        // Create the destination directory
        Directory.CreateDirectory(dstDir) |> ignore
        
        // skip "__oly" prefix
        if not(file.Name.StartsWith("__oly", StringComparison.OrdinalIgnoreCase)) then             
            let targetFilePath = Path.Combine(dstDir, file.Name)
            let targetFile = FileInfo(targetFilePath)
            if targetFile.Exists then
                if file.LastWriteTimeUtc > targetFile.LastWriteTimeUtc then                  
                    file.CopyTo(targetFilePath, true) |> ignore
            else
                file.CopyTo(targetFilePath) |> ignore

    static member CopyFileToDirectory(filePath: string, dstDir) =
        OlyIO.CopyFileToDirectory(FileInfo(filePath), dstDir)

    static member CopyDirectory(srcDir, dstDir) =
        let dir = DirectoryInfo(srcDir)
    
        // Cache directories before we start copying
        let dirs = dir.GetDirectories()

        // Create the destination directory
        Directory.CreateDirectory(dstDir) |> ignore

        // Get the files in the source directory and copy to the destination directory
        for file in dir.GetFiles() do
            OlyIO.CopyFileToDirectory(file, dstDir)

        for subDir in dirs do
            let newDestinationDir = Path.Combine(dstDir, subDir.Name)
            OlyIO.CopyDirectory(subDir.FullName, newDestinationDir)

    static member GetFilesFromDirectory(dir) =
        let files = ImArray.builder()
        let dir = DirectoryInfo(dir)

        // Get the files in the source directory and copy to the destination directory
        for file in dir.GetFiles() do
            // skip "__oly" prefix
            if not(file.Name.StartsWith("__oly", StringComparison.OrdinalIgnoreCase)) then
                files.Add(file.FullName)

        files.ToImmutable()

    static member GetLastWriteTimeUtc(filePath: OlyPath) =
        File.GetLastWriteTimeUtc(filePath.ToString())

    static member GetLastWriteTimeUtcOrDefault(filePath: OlyPath) =
        try OlyIO.GetLastWriteTimeUtc(filePath) with | _ -> DateTime()

    static member GetLastWriteTimeUtcFromDirectoryRecursively(dir) =
        let mutable dt = DateTime()

        OlyIO.GetFilesFromDirectory(dir)
        |> ImArray.iter (fun x ->
            let dtResult = File.GetLastWriteTimeUtc(x)
            if dtResult > dt then
                dt <- dtResult
        )

        for subDir in DirectoryInfo(dir).GetDirectories() do
            let newDestinationDir = Path.Combine(dir, subDir.Name)
            let dtResult = OlyIO.GetLastWriteTimeUtcFromDirectoryRecursively(subDir.FullName)
            if dtResult > dt then
                dt <- dtResult

        dt

    static member GetFileNamesFromDirectory(dir) =
        let files = ImArray.builder()
        let dir = DirectoryInfo(dir)

        // Get the files in the source directory and copy to the destination directory
        for file in dir.GetFiles() do
            // skip "__oly" prefix
            if not(file.Name.StartsWith("__oly", StringComparison.OrdinalIgnoreCase)) then
                files.Add(file.Name)

        files.ToImmutable()

    static member OpenFileRead(filePath: string) =
        try
            File.Open(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
        with
        | _ ->
            try
                Threading.Thread.Sleep(100)
                File.Open(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)
            with
            | _ ->
                Threading.Thread.Sleep(100)
                File.Open(filePath, FileMode.Open, FileAccess.Read, FileShare.Read)                

