namespace OpilioCraft.FSharp.CustomAttributes

open System
open Microsoft.FSharp.Reflection

/// Module for working with attributes on union cases.
module private AttributeSupport =
    /// Gets the first attribute of the specified type from a union case, if present.
    let getUnionCaseAttribute<'A when 'A :> Attribute> (value: obj) =
        let valueType = value.GetType()

        if FSharpType.IsUnion(valueType) then
            let unionCase, _ = FSharpValue.GetUnionFields(value, valueType)

            unionCase.GetCustomAttributes(typeof<'A>)
            |> Seq.cast<'A>
            |> Seq.tryHead

        else
            None // Not a union case, return None

module ReturnCode =
    /// Attribute for specifying a return code on a union case field.
    [<AttributeUsage(AttributeTargets.Property)>]
    type ReturnCodeAttribute(returnCode: int) =
        inherit Attribute()
        member val ReturnCode = returnCode

    /// Tries to get the return code from a union case, if the attribute is present.
    let tryGetReturnCode : obj -> int option =
        AttributeSupport.getUnionCaseAttribute<ReturnCodeAttribute> >> Option.map (fun attr -> attr.ReturnCode)
