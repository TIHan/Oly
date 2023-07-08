namespace Oly.Core

open System
open System.IO

/// A self-normalizing path for files and directories.
[<Struct;CustomEquality;NoComparison>]
type OlyPath private (path: string) =

    static let empty = OlyPath("")

    static member Empty = empty

    /// Does no computation, just simply returns the underlying path as a string.
    override _.ToString() = path

    override _.GetHashCode() = path.GetHashCode()

    override this.Equals(o) =
        match o with
        | :? OlyPath as o -> OlyPath.Equals(this, o)
        | _ -> false

    /// Not case sensitive.
    member this.StartsWith(str: string) =
        path.StartsWith(str, StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.StartsWith(path2: OlyPath) =
        path.StartsWith(path2.ToString(), StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.EndsWith(str: string) =
        path.EndsWith(str, StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.HasExtension(ext: string) =
        Path.GetExtension(path).Equals(ext, StringComparison.OrdinalIgnoreCase)

    member this.IsDirectory =
        path.EndsWith('/')

    member this.IsFile =
        not this.IsDirectory

    /// Tries to get glob information from a path i.e. "src/*.oly"
    /// which would return Some("src", ".oly").
    member this.TryGetGlob() =
        if this.IsDirectory then
            None
        elif path.Contains("*.") then
            let index = path.IndexOf("*")
            let dir = path.Substring(0, index)
            let ext = path.Substring(index + 1)
            Some(OlyPath.Create(dir), ext)
        else
            None

    member this.ContainsDirectoryOrFile(path: OlyPath) =
        match this.TryGetGlob() with
        | Some(dir, ext) ->
            if path.IsDirectory then
                let result = Path.GetRelativePath(dir.ToString(), path.ToString())
                if String.IsNullOrWhiteSpace(result) || result.StartsWith("..") then
                    false
                else
                    true
            elif path.HasExtension(ext) then
                let result = Path.GetRelativePath(dir.ToString(), path.ToString())
                if String.IsNullOrWhiteSpace(result) || result.StartsWith("..") then
                    false
                else
                    true
            else
                false
        | _ ->
            if this.ToString().Equals(path.ToString()) then
                true
            elif this.IsDirectory then
                let result = Path.GetRelativePath(this.ToString(), path.ToString())
                if String.IsNullOrWhiteSpace(result) || result.StartsWith("..") then
                    false
                else
                    true
            else
                false

    member this.ToDirectoryInfo() =
        if this.IsFile then
            failwith $"'{path}' is not a directory."
        DirectoryInfo(path)

    static member private NormalizeDirectory(dir: string) =
        if String.IsNullOrWhiteSpace dir then
            dir
        else
            if dir.StartsWith("\\\\") then
                dir.Substring(2, dir.Length - 2)
            elif dir.StartsWith("\\") then
                dir.Substring(1, dir.Length - 1)
            elif dir.StartsWith("/") then
                dir.Substring(1, dir.Length - 1)
            else
                dir
    
    static member private Normalize(path: string) =
        let path =
            if String.IsNullOrWhiteSpace path then
                path
            else
                if Path.EndsInDirectorySeparator(path) then
                    OlyPath.NormalizeDirectory(path).Replace("\\", "/")
                else
                    let fileNameWithExt = Path.GetFileName(path)
                    let dir = Path.GetDirectoryName(path) |> OlyPath.NormalizeDirectory
                    Path.Combine(dir, fileNameWithExt).Replace("\\", "/")
        if Path.IsPathFullyQualified(path) && not (path.EndsWith('/')) then
            // We do this to get rid of the ".."
            Path.GetFullPath(path).Replace("\\", "/")
        else
            path
    
    static member private NormalizeCombine(rootPath: string, path: string) =
        let dir = 
            if Path.HasExtension(rootPath) then
                Path.GetDirectoryName(rootPath) |> OlyPath.NormalizeDirectory
            else
                (OlyPath.Create rootPath).ToString()
        let newPath = Path.Combine(dir, path)
        OlyPath.Normalize(newPath)

    static member HasExtension(path: OlyPath) =
        Path.HasExtension(path.ToString())

    static member GetExtension(path: OlyPath) =
        Path.GetExtension(path.ToString())

    static member GetFileNameWithoutExtension(path: OlyPath) =
        Path.GetFileNameWithoutExtension(path.ToString())

    static member GetFileName(path: OlyPath) =
        Path.GetFileName(path.ToString())

    static member ChangeExtension(path: OlyPath, ext: string) =
        Path.ChangeExtension(path.ToString(), ext)
        |> OlyPath.Create

    static member GetDirectory(path: OlyPath) =
        (Path.GetDirectoryName(path.ToString()) + "/")
        |> OlyPath.Create

    static member GetRelativePath(relativeTo: OlyPath, path: OlyPath) =
        OlyPath.Create(Path.GetRelativePath(relativeTo.ToString(), path.ToString()))

    static member IsRooted(path: OlyPath) = 
        Path.IsPathRooted(path.ToString())

    static member Equals(path1: OlyPath, path2: OlyPath) =
        path1.ToString().Equals(path2.ToString(), StringComparison.OrdinalIgnoreCase)

    static member Combine(path1: OlyPath, path2: string) =
        OlyPath.NormalizeCombine(path1.ToString(), path2)
        |> OlyPath

    static member Combine(path1: OlyPath, path2: OlyPath) =
        OlyPath.NormalizeCombine(path1.ToString(), path2.ToString())
        |> OlyPath

    static member Create(path: string) : OlyPath =
        OlyPath.Normalize(path)
        |> OlyPath

[<Sealed>]
type OlyPathEqualityComparer private () =

    static member val Instance = OlyPathEqualityComparer()
    
    interface System.Collections.Generic.IEqualityComparer<OlyPath> with

        member this.GetHashCode(path) = path.ToString().GetHashCode()

        member this.Equals(path1, path2) = OlyPath.Equals(path1, path2)