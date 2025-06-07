namespace Oly.LanguageServer

open Oly.Core
open Oly.Compiler.Workspace

[<NoEquality;NoComparison>]
type OlySolutionTreeNodeViewModel =
    {
        id: string
        color: string
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
            id = doc.Path.ToString()
            color = ""
            label = OlyPath.GetFileName(doc.Path)
            description = ""
            resourcePath = doc.Path.ToString()
            tooltip = ""
            children = [||]
            collapsibleState = 0
            icon = "symbol-file"
        }

    static member FromDocuments(rootProjPath: OlyPath, docs: OlyDocument imarray): OlySolutionTreeNodeViewModel[] =
        let groups =
            docs
            |> Seq.groupBy (fun x -> OlyPath.GetRelativePath(OlyPath.GetDirectory(rootProjPath), OlyPath.GetDirectory(x.Path)).ToString())

        let mutable id = 0
        groups
        |> Seq.map (fun (dir, docs) ->
            let children = 
                docs
                |> Seq.filter (fun doc -> doc.Path.EndsWith(".olyx") |> not)
                |> Seq.map OlySolutionTreeNodeViewModel.FromDocument
                |> Array.ofSeq

            if dir = "." then
                children
            else
                id <- id + 1
                [|      
                    {
                        id = rootProjPath.ToString() + id.ToString()
                        color = ""
                        label = dir
                        description = ""
                        resourcePath = null
                        tooltip = ""
                        children = children
                        collapsibleState = if children.Length > 0 then 1 else 0
                        icon = "symbol-folder"
                    }
                |]
        )
        |> Seq.reduce Array.append
        |> Array.ofSeq

    static member FromProject(project: OlyProject): OlySolutionTreeNodeViewModel =
        let children = OlySolutionTreeNodeViewModel.FromDocuments(project.Path, project.Documents)
        {
            id = project.Path.ToString()
            color = "highlightForeground"
            label = project.Name
            description = ""
            resourcePath = project.Path.ToString()
            tooltip = ""
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
        

