module OpilioCraft.FSharp.Prelude

/// <summary>
/// Check if a value is null. Useful e.g. in context of JSON parsing.
/// </summary>
let inline isNull value = value = Unchecked.defaultof<_>

/// <summary>
/// Check if a value is not null. Useful e.g. in context of JSON parsing.
/// </summary>
let inline isNotNull value = not (isNull value)

/// <summary>
/// Short-hand for creating a new Error value.
/// </summary>
let inline (!!) details = Error details

/// <summary>
/// Short-hand for raising an exception with details.
/// </summary>
let inline (!!!) exn details = raise (exn details)

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

/// <summary>
/// Applys a side effect only if cond is false. Returns the value without changing it.
/// </summary>
/// <param name="cond">The condition.</param>
/// <param name="x">The value.</param>
let inline teeIfNot cond f x = teeP (fun _ -> cond = false) f x

/// <summary>
/// Applies a function to a value if the predicate is true, otherwise returns the value unchanged.
/// </summary>
let inline applyIf p f x = if p x then f x else x

/// <summary>
/// Applies a function to a value if the predicate is false, otherwise returns the value unchanged.
/// </summary>
let inline applyIfNot p f x = if not (p x) then f x else x
