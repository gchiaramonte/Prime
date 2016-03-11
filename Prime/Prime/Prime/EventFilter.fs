﻿namespace Prime
open System
open System.ComponentModel
open System.Text.RegularExpressions

/// Converts Rexpr types.
type RexprConverter () =
    inherit TypeConverter ()

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = typeof<Rexpr>

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then scstring source :> obj 
        elif destType = typeof<Symbol> then
            let rexpr = source :?> Rexpr
            Atom ^ scstring rexpr :> obj
        elif destType = typeof<Rexpr> then source
        else failwith "Invalid RexprConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = typeof<Rexpr>

    override this.ConvertFrom (_, _, source) =
        match source with
        | :? string as str -> scvalue<Rexpr> str :> obj
        | :? Symbol as symbol ->
            match symbol with
            | Atom pattern -> Rexpr pattern :> obj
            | Quote _ -> failwith "Expected Atom value for conversion to string."
            | Symbols _ -> failwith "Expected Atom value for conversion to string."
        | :? Rexpr -> source
        | _ -> failwith "Invalid RexprConverter conversion from source."

/// Effectively new-types the Regex type to implement custom type-conversation without needing
/// explicit initialization by the client program.
and [<TypeConverter (typeof<RexprConverter>)>] Rexpr (pattern) =
    inherit Regex (pattern)

[<RequireQualifiedAccess>]
module EventFilter =

    /// Describes how events are filtered.
    type [<ReferenceEquality>] EventFilter =
        | Any of EventFilter list
        | All of EventFilter list
        | None of EventFilter list
        | Pattern of Rexpr * Rexpr list
        | Empty

    /// Filter events.
    let rec filter addressStr traceRev eventFilter =
        match eventFilter with
        | EventFilter.Any exprs -> List.fold (fun passed eventFilter -> passed || filter addressStr traceRev eventFilter) false exprs
        | EventFilter.All exprs -> List.fold (fun passed eventFilter -> passed && filter addressStr traceRev eventFilter) true exprs
        | EventFilter.None exprs -> not ^ List.fold (fun passed eventFilter -> passed || filter addressStr traceRev eventFilter) false exprs
        | EventFilter.Pattern (addressRexpr, traceRexpr) ->
            if addressRexpr.IsMatch addressStr then
                let mutable passes = true
                let mutable enr = enumerator traceRexpr
                for eventInfo in traceRev do
                    if passes && enr.MoveNext () then
                        passes <- enr.Current.IsMatch (scstring eventInfo)
                passes
            else false
        | EventFilter.Empty -> true

[<AutoOpen>]
module EventFilterModule =

    /// Describes how events are filtered.
    type EventFilter = EventFilter.EventFilter