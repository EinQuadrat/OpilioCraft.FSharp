namespace OpilioCraft.FSharp.Prelude

exception FlexibleValueException of ErrorMsg:string
    with override x.Message = $"[FlexibleValue] {x.ErrorMsg}"

[<RequireQualifiedAccess>]
type FlexibleValue =
    // primitive types
    | Boolean of bool
    | Numeral of int
    | Float of float
    | Decimal of decimal
    | String of string

    // object types
    | Date of System.DateOnly
    | Time of System.TimeOnly
    | DateTime of System.DateTime
    | TimeSpan of System.TimeSpan

    with
        override x.ToString () =
            match x with
            | Boolean boolValue -> boolValue.ToString()
            | Numeral numValue -> numValue.ToString()
            | Float numValue -> numValue.ToString()
            | Decimal numValue -> numValue.ToString()
            | String stringValue -> stringValue
            | Date dateValue -> dateValue.ToString("yyyy-MM-dd")
            | Time timeValue -> timeValue.ToString("HH:mm:ss")
            | DateTime datetimeValue -> datetimeValue.ToString("yyyy-MM-ddTHH:mm:ss")
            | TimeSpan timespanValue -> timespanValue.ToString("c")
        
        static member inline TryWrap (incoming : 'a) : FlexibleValue option =
            match box incoming with
            | :? FlexibleValue as flexValue       -> Some flexValue // prevent nesting
            | :? System.Boolean as boolValue      -> Some (Boolean boolValue)
            | :? System.Int32 as intValue         -> Some (Numeral intValue)
            | :? System.Double as floatValue      -> Some (Float floatValue)
            | :? System.Decimal as decimalValue   -> Some (Decimal decimalValue)
            | :? System.String as stringValue     -> Some (String stringValue)
            | :? System.DateOnly as dateValue     -> Some (Date dateValue)
            | :? System.TimeOnly as timeValue     -> Some (Time timeValue)
            | :? System.DateTime as datetimeValue -> Some (DateTime datetimeValue)
            | :? System.TimeSpan as timespanValue -> Some (TimeSpan timespanValue)
            | _ -> None

        static member inline Wrap (incoming : 'a) : FlexibleValue =
            FlexibleValue.TryWrap incoming
            |> Option.defaultWith (fun _ -> raise <| FlexibleValueException $"unsupported support incoming type: {incoming.GetType().FullName}")

        static member inline WrapOrStringify (incoming : 'a) : FlexibleValue =
            FlexibleValue.TryWrap incoming
            |> Option.defaultValue (String (incoming.ToString()))

        member x.Unwrap : obj =
            match x with
            // primitive types
            | Boolean plainValue  -> plainValue :> obj
            | Numeral plainValue  -> plainValue :> obj
            | Float plainValue    -> plainValue :> obj
            | Decimal plainValue  -> plainValue :> obj
            | String plainValue   -> plainValue :> obj
            
            // object types
            | DateTime plainValue -> plainValue :> obj
            | Date plainValue     -> plainValue :> obj
            | Time plainValue     -> plainValue :> obj
            | TimeSpan plainValue -> plainValue :> obj

        member x.TryCompareTo value : int option =
            let compareFlexibleValues (fv1 : FlexibleValue) (fv2 : FlexibleValue) : int option =
                match (fv1, fv2) with
                    | Boolean a, Boolean b -> a.CompareTo(b) |> Some
                    | Numeral a, Numeral b -> a.CompareTo(b) |> Some
                    | Decimal a, Decimal b -> a.CompareTo(b) |> Some
                    | String a, String b -> System.String.Compare(a, b) |> Some
                    | Date a, Date b -> a.CompareTo(b) |> Some
                    | Time a, Time b -> a.CompareTo(b) |> Some
                    | DateTime a, DateTime b -> a.CompareTo(b) |> Some
                    | _ -> None // comparison is not supported

            match box value with
                | :? FlexibleValue as flexValue -> compareFlexibleValues x flexValue
                | other ->
                    try
                        FlexibleValue.TryWrap other
                        |> Option.bind (fun value -> compareFlexibleValues x value)
                    with
                        | _ -> None

        // explicit castings
        member x.AsBoolean : bool =
            match x with
            | Boolean value -> value
            | _ -> failwith $"cannot cast {x.GetType().FullName} to System.Boolean"

        member x.AsDateTime : System.DateTime =
            match x with
            | DateTime value -> value
            | _ -> failwith $"cannot cast {x.GetType().FullName} to System.DateTime"

        member x.AsString : string =
            match x with
            | String value -> value
            | _ -> failwith $"cannot cast {x.GetType().FullName} to System.String"
