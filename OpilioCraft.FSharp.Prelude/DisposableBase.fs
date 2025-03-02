namespace OpilioCraft.FSharp.Prelude

open System

/// <summary>Simply correct implementation of IDisposable.</summary>
/// <para>
/// Types can inherit from <c>DisposableBase</c> to ensure correct implementation
/// of the IDisposable pattern. </para>
[<AbstractClass>]
type DisposableBase() =
    let mutable _isDisposed = false

    /// Overwrite this method to cleanup managed resources.
    abstract member DisposeManagedResources : unit -> unit
    default _.DisposeManagedResources () = ignore ()

    /// Overwrite this method to cleanup native resources.
    abstract member FreeNativeResources : unit -> unit
    default _.FreeNativeResources () = ignore ()

    /// Entry point to provide the action to be run for dispose.
    /// <param name="disposing">is <c>true</c> to indicate that method was called from IDisposable.Dispose();
    /// <c>false</c> if called from finalizer </param>
    member x.Dispose disposing  =
        if not _isDisposed then
            try
                if disposing then x.DisposeManagedResources()
            with
                | exn -> Console.Error.WriteLine $"[{nameof DisposableBase}] disposal of managed resources failed: {exn.Message}"

            try
                x.FreeNativeResources()
            with
                | exn -> Console.Error.WriteLine $"[{nameof DisposableBase}] disposal of unmanaged resources failed: {exn.Message}"

            _isDisposed <- true // in case of error: do not try again

    /// Simplify invocation of method <c>IDisposable.Dispose()</c>, avoiding type cast to IDisposable.
    member x.Dispose () = (x :> IDisposable).Dispose()

    /// Implements the interface IDisposable
    interface IDisposable with
        member x.Dispose () =
            x.Dispose true
            GC.SuppressFinalize(x)

    /// Disposing of unmanaged resources
    override x.Finalize () =
        x.Dispose false
