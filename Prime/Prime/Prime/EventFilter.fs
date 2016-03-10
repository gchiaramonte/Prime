namespace Prime
open System
open System.ComponentModel
open System.Text.RegularExpressions

/// Converts EventPattern types.
type EventPatternConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = typeof<EventPattern>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then scstring source :> obj 
        elif destType = typeof<Symbol> then
            let ef = source :?> EventPattern
            let af = ef.AddressPattern |> scstring |> Atom
            let tf = ef.TracePattern |> List.map (scstring >> Atom) |> Symbols
            Symbols [af; tf] :> obj
        elif destType = typeof<EventPattern> then source
        else failwith "Invalid EventPatternConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = typeof<EventPattern>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? Symbol as symbol ->
            match symbol with
            | Symbols [Atom af; Symbols tf] ->
                let afr = Regex af
                let tfr = List.map (function Atom atom -> Regex atom | _ -> failwithumf ()) tf
                { AddressPattern = afr; TracePattern = tfr } :> obj
            | _ -> failwith "Invalid EventPatternConverter conversion from source."
        | :? string as str -> scvalue<EventPattern> str :> obj
        | :? EventPattern -> source
        | _ -> failwith "Invalid EventPatternConverter conversion from source."

and [<ReferenceEquality; TypeConverter (typeof<EventPatternConverter>)>] EventPattern =
    { AddressPattern : Regex
      TracePattern : Regex list }

[<RequireQualifiedAccess>]
module EventFilter =

    type [<ReferenceEquality>] EventFilter =
        | Or of EventFilter list
        | Nor of EventFilter list
        | And of EventFilter list
        | Nand of EventFilter list
        | Pattern of EventPattern
        | Empty

    /// Query the an event should be logged
    let rec shouldLog address trace expr =
        match expr with
        | EventFilter.Or exprs -> List.fold (fun passed expr -> passed || shouldLog address trace expr) false exprs
        | EventFilter.Nor exprs -> List.fold (fun passed expr -> not passed && not (shouldLog address trace expr)) false exprs
        | EventFilter.And exprs -> List.fold (fun passed expr -> passed && shouldLog address trace expr) true exprs
        | EventFilter.Nand exprs -> List.fold (fun passed expr -> not (passed && shouldLog address trace expr)) true exprs
        | EventFilter.Pattern pattern ->
            let addressStr = scstring address
            if pattern.AddressPattern.IsMatch addressStr then
                let mutable passes = true
                let mutable enr = enumerator pattern.TracePattern
                for eventInfo in trace do
                    if passes && enr.MoveNext () then
                        passes <- enr.Current.IsMatch (scstring eventInfo)
                passes
            else false
        | EventFilter.Empty -> true

[<AutoOpen>]
module EventFilterModule =

    type EventFilter = EventFilter.EventFilter