namespace Prime
open System
open System.ComponentModel
open System.Text.RegularExpressions

/// Converts EventFilter types.
type EventFilterConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = typeof<EventFilter>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then scstring source :> obj 
        elif destType = typeof<Symbol> then
            let ef = source :?> EventFilter
            let af = ef.AddressFilter |> scstring |> Atom
            let tf = ef.TraceFilter |> List.map (scstring >> Atom) |> Symbols
            Symbols [af; tf] :> obj
        elif destType = typeof<EventFilter> then source
        else failwith "Invalid EventFilterConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = typeof<EventFilter>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols [Atom af; Symbols tf] ->
                let afr = Regex af
                let tfr = List.map (function Atom atom -> Regex atom | _ -> failwithumf ()) tf
                { AddressFilter = afr; TraceFilter = tfr } :> obj
            | _ -> failwith "Invalid EventFilterConverter conversion from source."
        | :? string as str -> scvalue<EventFilter> str :> obj
        | :? EventFilter -> source
        | _ -> failwith "Invalid EventFilterConverter conversion from source."

and [<ReferenceEquality; TypeConverter (typeof<EventFilterConverter>)>] EventFilter =
    { AddressFilter : Regex
      TraceFilter : Regex list }

type EventFilters = EventFilter list

type [<CLIMutable; ReferenceEquality>] EventInfo =
    { ModuleName : string
      FunctionName : string
      MoreInfo : string }

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module EventInfo =

    let record moduleName functionName =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = String.Empty }

    let record3 moduleName functionName moreInfo =
        { ModuleName = moduleName
          FunctionName = functionName
          MoreInfo = moreInfo }

[<AutoOpen>]
module EventTrace =

    type EventTrace = EventInfo list

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EventTrace =

        let filter (eventFilter : EventFilter) (eventAddress : 'a Address) (eventTrace : EventTrace) =
            let addressStr = scstring eventAddress
            if eventFilter.AddressFilter.IsMatch addressStr then
                let mutable passes = true
                let mutable enr = (eventFilter.TraceFilter :> _ seq).GetEnumerator ()
                for eventInfo in eventTrace do
                    if passes && enr.MoveNext () then
                        passes <- enr.Current.IsMatch (scstring eventInfo)
                passes
            else false

        let record moduleName functionName eventTrace : EventTrace =
            EventInfo.record moduleName functionName :: eventTrace

        let record4 moduleName functionName moreInfo eventTrace : EventTrace =
            EventInfo.record3 moduleName functionName moreInfo :: eventTrace

        let empty : EventTrace =
            []