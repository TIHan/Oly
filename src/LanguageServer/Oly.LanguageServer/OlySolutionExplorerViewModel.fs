namespace Oly.LanguageServer

open Oly.Core
open Oly.Compiler.Workspace

[<AutoOpen>]
module private OlySolutionTreeHelpers =

    let getDirectoryParts (dirInfo: OlyPath) =
        dirInfo.ToString().Split('/')
        |> Array.filter (fun x -> System.String.IsNullOrWhiteSpace(x) |> not)

[<NoEquality;NoComparison>]
type OlySolutionTreeNodeViewModel =
    {
        id: string
        label: string
        description: string
        resourcePath: string
        tooltip: string
        children: OlySolutionTreeNodeViewModel[]
        collapsibleState: int
        icon: string
    }

    static member FromDocument(doc: OlyDocument): OlySolutionTreeNodeViewModel =
        {
            id = doc.Path.ToString() + $"({doc.Project.SharedBuild.PlatformName})({doc.Project.TargetInfo.Name})({doc.Project.Name})"
            label = OlyPath.GetFileName(doc.Path)
            description = ""
            resourcePath = doc.Path.ToString()
            tooltip = ""
            children = [||]
            collapsibleState = 0
            icon = "symbol-file"
        }

    static member FromDocuments(projectDir: OlyPath, docs: OlyDocument imarray): OlySolutionTreeNodeViewModel[] =
        let groups =
            docs
            |> Seq.groupBy (fun x -> 
                OlyPath.GetRelativePath(OlyPath.GetDirectory(projectDir), OlyPath.GetDirectory(x.Path)).ToString()
            )

        let lookup = System.Collections.Generic.Dictionary<string, obj>(System.StringComparer.OrdinalIgnoreCase)       

        groups
        |> Seq.iter (fun (dir, docs) ->
            let dirParts = getDirectoryParts (OlyPath.Create(dir))
            let lastIndex = dirParts.Length - 1

            let mutable lookup = lookup
            for i = 0 to lastIndex do
                let key = dirParts[i]
                let xs, next =
                    match lookup.TryGetValue key with
                    | true, x -> x :?> (ResizeArray<OlySolutionTreeNodeViewModel> * System.Collections.Generic.Dictionary<string, obj>)
                    | _ ->
                        let xs = ResizeArray<OlySolutionTreeNodeViewModel>()
                        let next = System.Collections.Generic.Dictionary<string, obj>()
                        lookup[key] <- (xs, next)
                        xs, next
                if i = lastIndex then
                    docs
                    |> Seq.iter (fun doc ->
                        if not(doc.Path.EndsWith(".olyx")) then
                            xs.Add(OlySolutionTreeNodeViewModel.FromDocument(doc))
                    )
                else
                    lookup <- next

        )

        let mutable id = 0

        let rec getFolderNodes (lookup: System.Collections.Generic.Dictionary<string, obj>) =
            lookup
            |> Seq.map (fun pair ->
                let folder = pair.Key
                let xs, next = pair.Value :?> (ResizeArray<OlySolutionTreeNodeViewModel> * System.Collections.Generic.Dictionary<string, obj>)

                let children =
                    [|
                        if next.Count > 0 then
                            yield! getFolderNodes next
                        yield! xs
                    |]
                    |> Array.sortBy (fun x -> x.label)
                    |> Array.sortByDescending (fun x -> x.icon)

                if folder = "." then
                    children
                else
                    id <- id + 1
                    [|
                        yield {
                            id = projectDir.ToString() + id.ToString()
                            label = folder
                            description = ""
                            resourcePath = null
                            tooltip = ""
                            children = children |> Array.ofSeq
                            collapsibleState = if Array.isEmpty children then 0 else 1
                            icon = "symbol-folder"
                        }
                    |]
            )
            |> Seq.reduce Array.append
        getFolderNodes lookup
        |> Array.sortBy (fun x -> x.label)
        |> Array.sortByDescending (fun x -> x.icon)
        |> Array.ofSeq

    static member FromProject(project: OlyProject): OlySolutionTreeNodeViewModel =
        let children = OlySolutionTreeNodeViewModel.FromDocuments(OlyPath.GetDirectory(project.Path),  project.Documents)
        let platformString = $"({project.SharedBuild.PlatformName}: {project.TargetInfo.Name})"
        {
            id = project.Path.ToString() + platformString
            label = OlyPath.GetFileName(project.Path)
            description = ""
            resourcePath = project.Path.ToString()
            tooltip = project.Name + " " + platformString
            children = children
            collapsibleState = if children.Length > 0 then 1 else 0
            icon = "project"
        }

[<NoEquality;NoComparison>]
type OlySolutionExplorerViewModel =
    {
        children: OlySolutionTreeNodeViewModel[]
    }

    static member FromSolution(solution: OlySolution): OlySolutionExplorerViewModel =
        {
            children =
                solution.GetProjects()
                |> Seq.map (OlySolutionTreeNodeViewModel.FromProject)
                |> Seq.sortBy (_.label)
                |> Array.ofSeq
        }
        

