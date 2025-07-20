namespace Oly.Core

open System
open System.IO

/// A self-normalizing path for files and directories.
[<Struct;CustomEquality;NoComparison>]
type OlyPath private (innerPath: string) =

    static let empty = OlyPath("")

    static member Empty = empty

    member _.IsEmpty = innerPath = ""

    /// Does no computation, just simply returns the underlying path as a string.
    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
    override _.ToString() = innerPath

    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
    override _.GetHashCode() = innerPath.GetHashCode()

    [<Obsolete("This is slow, do not call this.", true)>]
    override this.Equals(_) = invalidOp $"Cannot call '{nameof(Object.Equals)}' on '{nameof(OlyPath)}' as it is not optimal."

    /// Not case sensitive.
    member this.StartsWith(str: string) =
        innerPath.StartsWith(str, StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.StartsWith(path2: OlyPath) =
        innerPath.StartsWith(path2.ToString(), StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.EndsWith(str: string) =
        innerPath.EndsWith(str, StringComparison.OrdinalIgnoreCase)

    /// Not case sensitive.
    member this.HasExtension(ext: string) =
        Path.GetExtension(innerPath).Equals(ext, StringComparison.OrdinalIgnoreCase)

    member _.GetExtension() =
        Path.GetExtension(innerPath)

    member _.GetFileNameWithoutExtension() =
        Path.GetFileNameWithoutExtension(innerPath)

    member _.GetFileName() =
        Path.GetFileName(innerPath)

    member _.ChangeExtension(ext: string) =
        Path.ChangeExtension(innerPath, ext)
        |> OlyPath.Create

    member _.GetDirectory() =
        (Path.GetDirectoryName(innerPath) + "/")
        |> OlyPath.Create

    member _.GetRelative(relativeTo: OlyPath) =
        OlyPath.Create(Path.GetRelativePath(relativeTo.ToString(), innerPath))

    member _.Join(path: string) =
        OlyPath.NormalizeCombine(innerPath, path)
        |> OlyPath

    member _.Join(path: OlyPath) =
        OlyPath.NormalizeCombine(innerPath, path.ToString())
        |> OlyPath

    member this.IsDirectory =
        innerPath.EndsWith('/')

    member this.IsFile =
        not this.IsDirectory

    /// Tries to get glob information from a path i.e. "src/*.oly"
    /// which would return Some("src", ".oly").
    member this.TryGetGlob() =
        if this.IsDirectory then
            None
        elif innerPath.Contains("*.") then
            let index = innerPath.IndexOf("*")
            let dir = innerPath.Substring(0, index)
            let ext = innerPath.Substring(index + 1)
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
            if OlyPath.Equals(this, path) then
                true
            elif this.IsDirectory then
                let result = Path.GetRelativePath(this.ToString(), path.ToString())
                if String.IsNullOrWhiteSpace(result) || result.StartsWith("..") then
                    false
                else
                    true
            else
                false

    member this.IsRooted = Path.IsPathRooted(this.ToString())

    member this.ToDirectoryInfo() =
        if this.IsFile then
            failwith $"'{innerPath}' is not a directory."
        DirectoryInfo(innerPath)

    member this.ToAbsolute() =
        if Path.IsPathRooted(this.ToString()) then
            this
        else
            OlyPath.Create(Environment.CurrentDirectory).Join(this)

    interface IEquatable<OlyPath> with
        [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
        member _.Equals (other: OlyPath): bool = 
            innerPath.Equals(other.ToString(), StringComparison.OrdinalIgnoreCase)

    static member private NormalizeDirectory(dir: string) =
        if System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(System.Runtime.InteropServices.OSPlatform.Windows) then
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

    [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
    static member Equals(path1: OlyPath, path2: OlyPath) =
        path1 = path2

    static member Create(path: string) : OlyPath =
        OlyPath.Normalize(path)
        |> OlyPath

    static member CreateAbsolute(path: string) =
        Path.GetFullPath(path)
        |> OlyPath.Create

[<Sealed>]
type OlyPathEqualityComparer private () =

    static member val Instance = OlyPathEqualityComparer()
    
    interface System.Collections.Generic.IEqualityComparer<OlyPath> with

        [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
        member this.GetHashCode(path) = path.GetHashCode()

        [<System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.AggressiveInlining)>]
        member this.Equals(path1, path2) = path1 = path2