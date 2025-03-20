namespace OpilioCraft.FSharp.PowerShell

open System
open System.Management.Automation
open CmdletExtension

[<AbstractClass>]
[<CmdletBinding(DefaultParameterSetName="Path")>]
type public PathSupportingCommand () =
    inherit PSCmdlet ()

    // parameters
    [<Parameter(Position=0, Mandatory=true, ValueFromPipeline=true, ValueFromPipelineByPropertyName=true, ParameterSetName="Path")>]
    [<ValidateNotNullOrEmpty>]
    [<SupportsWildcards>]
    member val Path : String array = [| |] with get, set

    [<Parameter(Mandatory=true, ValueFromPipelineByPropertyName=true, ParameterSetName="LiteralPath")>]
    [<ValidateNotNullOrEmpty>]
    [<Alias("PSPath")>]
    member val LiteralPath : String array = [| |] with get, set

    // cmdlet behaviour
    member x.TypedParameterSet =
        match x.ParameterSetName with
        | name when name.StartsWith("Path") -> Path name
        | name when name.StartsWith("LiteralPath") -> LiteralPath name
        | name -> Other name

    member x.ResolvePath = x.GetResolvedProviderPathFromPSPath >> fst

    override x.ProcessRecord() =
        base.ProcessRecord()

        try
            match x.TypedParameterSet with
            | Path _ ->
                Seq.collect x.ResolvePath x.Path
                |> Seq.map (Assert.fileExists "given path does not exist or is not accessible")
                |> Seq.iter x.ProcessPath

            | LiteralPath _ ->
                seq { for p in x.LiteralPath -> x.GetUnresolvedProviderPathFromPSPath p }
                |> Seq.map (Assert.fileExists "given path does not exist or is not accessible")
                |> Seq.iter x.ProcessPath

            | _ -> 
                x.ProcessNonPath ()

        with
            | exn -> exn |> x.WriteAsError ErrorCategory.NotSpecified

    abstract member ProcessPath : string -> unit
    abstract member ProcessNonPath : unit -> unit

and TypedParameterSet =
    | Path of string
    | LiteralPath of string
    | Other of string

    member x.IsPathLike = match x with | Other _ -> false | _ -> true
    member x.IsNotPathLike = not x.IsPathLike

// ------------------------------------------------------------------------------------------------

[<AbstractClass>]
[<CmdletBinding(DefaultParameterSetName="Path")>]
type public PathExpectingCommand () =
    inherit PathSupportingCommand ()

    override _.ProcessNonPath() = raise <| InvalidOperationException()
