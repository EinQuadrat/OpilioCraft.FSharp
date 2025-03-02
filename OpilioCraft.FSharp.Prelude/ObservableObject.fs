namespace OpilioCraft.FSharp.Prelude

open System.ComponentModel

// Property enabled object
type ObservableObject () =
    // Helper method to support quotations form
    let getPropertyName = function 
        | Quotations.Patterns.PropertyGet(_, pi, _) -> pi.Name
        | _ -> invalidOp "Expecting property getter expression"

    // Implement the INotifyPropertyChanged interface
    let _event_PropertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member _.PropertyChanged = _event_PropertyChanged.Publish

    // Make it easier to use this interface ;-)
    member this.PropertyChanged = (this :> INotifyPropertyChanged).PropertyChanged

    // Notification methods
    member this.RaisePropertyChanged propertyName = 
        _event_PropertyChanged.Trigger(this, PropertyChangedEventArgs(propertyName))
        // USAGE: this.RaisePropertyChanged "nameOfProperty"

    member this.RaisePropertyChanged quotation = 
        quotation |> getPropertyName |> this.RaisePropertyChanged
        // USAGE: this.RaisePropertyChanged <@ this.Property @>
