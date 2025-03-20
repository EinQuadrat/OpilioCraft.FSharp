namespace OpilioCraft.FSharp.Prelude

[<AutoOpen>]
module Prelude =
    /// <summary>
    /// Check if a value is null. Useful e.g. in context of JSON parsing.
    /// </summary>
    let inline isNull value = value = Unchecked.defaultof<_>

    /// <summary>
    /// Check if a value is not null. Useful e.g. in context of JSON parsing.
    /// </summary>
    let inline isNotNull value = not (isNull value)

    /// <summary>
    /// Simplifies verification of critical conditions.
    /// </summary>
    let inline ( -||- ) condition exn = if not condition then raise exn

    /// <summary>
    /// Allows applying a side effect without changing the value. Same as operator |>!.
    /// </summary>
    /// <param name="x">The value.</param>
    /// <param name="f">The side effect to apply.</param>
    let inline tee f x = ignore(f x); x

    /// <summary>
    /// Allows applying a side effect without changing the value. Same as tee.
    /// </summary>
    /// <param name="x">The value.</param>
    /// <param name="f">The side effect to apply.</param>
    let inline ( |>! ) x f = tee f x

    /// <summary>
    /// Applys a side effect without changing the value, if the predicate on x is true.
    /// </summary>
    /// <param name="p">The predicate.</param>
    /// <param name="x">The value.</param>
    /// <param name="f">The side effect to apply.</param>
    let inline teeP p f x = ignore(if p x then f x) ; x

    /// <summary>
    /// Applys a side effect only if cond is true. Returns the value without changing it.
    /// </summary>
    /// <param name="cond">The condition.</param>
    /// <param name="x">The value.</param>
    /// <param name="f">The side effect to apply.</param>
    let inline teeIf cond f x = teeP (fun _ -> cond = true) f x
    