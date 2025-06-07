namespace Oly.LanguageServer

[<NoEquality;NoComparison>]
type OlySolutionTreeNodeViewModel =
    {
        label: string
        children: OlySolutionTreeNodeViewModel[]
    }

[<NoEquality;NoComparison>]
type OlySolutionTreeViewModel =
    {
        children: OlySolutionTreeNodeViewModel[]
    }

