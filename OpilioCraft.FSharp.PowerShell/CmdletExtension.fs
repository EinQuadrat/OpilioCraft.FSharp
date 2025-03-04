module OpilioCraft.FSharp.PowerShell.CmdletExtension

open System.Management.Automation
open ExceptionExtension

type System.Management.Automation.PSCmdlet with
    
    // emit warnings
    member x.WarnIfNone warning = fun o -> (if o |> Option.isNone then x.WriteWarning warning) ; o
    member x.WarnIfFalse warning value = (if not value then x.WriteWarning warning) ; value

    // simplify error handling
    member x.WriteAsError errorCategory (exn : #System.Exception) : unit =
        exn.ToError(errorCategory, x)
        |> x.WriteError

    member x.ThrowAsTerminatingError errorCategory (exn : #System.Exception) =
        exn.ToError(errorCategory, x)
        |> x.ThrowTerminatingError

    member x.ThrowValidationError errorMessage errorCategory =
        errorMessage
        |> ParameterBindingException
        |> x.ThrowAsTerminatingError errorCategory
