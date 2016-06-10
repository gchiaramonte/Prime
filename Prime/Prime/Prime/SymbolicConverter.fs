﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Collections
open System.Collections.Generic
open System.ComponentModel
open System.Reflection
open Microsoft.FSharp.Reflection
open Prime

/// Compresses two unions into a single union in a symbolic-expression.
type SymbolicCompression<'a, 'b> =
    | SymbolicCompressionA of 'a
    | SymbolicCompressionB of 'b

type SymbolicConverter (pointType : Type) =
    inherit TypeConverter ()

    // NOTE: had to do some reflection hacking get this assembly as it was the only way I could
    // access ListModule.OfSeq dynamically.
    static let FSharpCoreAssembly =
        Array.find
            (fun (assembly : Assembly) -> assembly.FullName.StartsWith ("FSharp.Core,", StringComparison.Ordinal))
            (AppDomain.CurrentDomain.GetAssemblies ())

    let objToObjList (source : obj) =
        let iEnumerable = source :?> IEnumerable
        List.ofSeq ^ enumerable<obj> iEnumerable

    let objToKeyValuePair (source : obj) =
        let kvpType = source.GetType ()
        let key = (kvpType.GetProperty "Key").GetValue (source, null)
        let value = (kvpType.GetProperty "Value").GetValue (source, null)
        KeyValuePair (key, value)

    let objToComparableSet (source : obj) =
        let iEnumerable = source :?> IEnumerable
        Set.ofSeq ^ enumerable<IComparable> iEnumerable

    let defaultify (fieldType : obj) (optValue : obj) =
        match optValue with
        | null -> optValue
        | _ -> let fieldType = fieldType :?> PropertyInfo in fieldType.PropertyType.GetDefaultValue ()
        
    let padWithDefaults (fieldTypes : PropertyInfo array) (values : obj list) =
        let valuesLength = List.length values
        if fieldTypes.Length = valuesLength then Array.ofList values
        else
            let values = List.pad (fieldTypes.Length - valuesLength) null values
            (fieldTypes |> Seq.map objectify |> List.ofSeq, values) ||> Seq.map2 defaultify |> Array.ofSeq

    let defaultify' (fieldType : obj) (optValue : obj) =
        match optValue with
        | null -> optValue
        | _ -> let fieldType = fieldType :?> Type in fieldType.GetDefaultValue ()

    let padWithDefaults' (fieldTypes : Type array) (values : obj list) =
        let valuesLength = List.length values
        if fieldTypes.Length = valuesLength then Array.ofList values
        else
            let values = List.pad (fieldTypes.Length - valuesLength) null values
            (fieldTypes |> Seq.map objectify |> List.ofSeq, values) ||> Seq.map2 defaultify' |> Array.ofSeq

    let rec toSymbol (sourceType : Type) (source : obj) =
        match sourceType.TryGetCustomTypeConverter () with
        | Some typeConverter ->

            // symbolize user-defined type
            if not ^ typeConverter.CanConvertTo typeof<Symbol>
            then failwith ^ "Cannot convert type '" + getTypeName source + "' to Prime.Symbol."
            else typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
        
        | None ->

            // symbolize .NET primitive
            if sourceType.IsPrimitive then
                let converted = (TypeDescriptor.GetConverter sourceType).ConvertTo (source, typeof<string>) :?> string
                if sourceType = typeof<bool> then Atom (converted, None)
                elif sourceType = typeof<char> then String (converted, None)
                else Number (converted, None)

            // symbolize string
            elif sourceType = typeof<string> then
                let sourceStr = string source
                if Symbol.shouldBeExplicit sourceStr then String (sourceStr, None)
                else Atom (sourceStr, None)

            // symbolize Symbol (no transformation)
            elif sourceType = typeof<Symbol> then
                source :?> Symbol

            // symbolize KeyValuePair
            elif sourceType.Name = typedefof<KeyValuePair<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let kvp = objToKeyValuePair source
                let keySymbol = toSymbol gargs.[0] kvp.Key
                let valueSymbol = toSymbol gargs.[1] kvp.Value
                Symbols ([keySymbol; valueSymbol], None)

            // symbolize array
            elif sourceType.Name = typedefof<_ array>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToObjList source
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, None)

            // symbolize list
            elif sourceType.Name = typedefof<_ list>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToObjList source
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, None)

            // symbolize Set
            elif sourceType.Name = typedefof<_ Set>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let items = objToComparableSet source |> List.ofSeq
                let symbols = List.map (toSymbol gargs.[0]) items
                Symbols (symbols, None)

            // symbolize Map
            elif sourceType.Name = typedefof<Map<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<KeyValuePair<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Symbols (symbols, None)

            // symbolize Vmap
            elif sourceType.Name = typedefof<Vmap<_, _>>.Name then
                let gargs = sourceType.GetGenericArguments ()
                let itemType = typedefof<Tuple<_, _>>.MakeGenericType [|gargs.[0]; gargs.[1]|]
                let items = objToObjList source
                let symbols = List.map (toSymbol itemType) items
                Symbols (symbols, None)

            // symbolize SymbolicCompression
            elif sourceType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let value = unionFields.[0]
                let valueType = value.GetType ()
                if unionCase.Tag = 0 then toSymbol valueType value
                else
                    let (_, unionFields) = FSharpValue.GetUnionFields (value, valueType)
                    let value = unionFields.[0]
                    let valueType = value.GetType ()
                    toSymbol valueType value

            // symbolize Tuple
            elif FSharpType.IsTuple sourceType then
                let tupleFields = FSharpValue.GetTupleFields source
                let tupleElementTypes = FSharpType.GetTupleElements sourceType
                let tupleFieldSymbols = List.mapi (fun i tupleField -> toSymbol tupleElementTypes.[i] tupleField) (List.ofArray tupleFields)
                Symbols (tupleFieldSymbols, None)

            // symbolize Record
            elif FSharpType.IsRecord sourceType then
                let recordFields = FSharpValue.GetRecordFields source
                let recordFieldTypes = FSharpType.GetRecordFields sourceType
                let recordFieldSymbols = List.mapi (fun i recordField -> toSymbol recordFieldTypes.[i].PropertyType recordField) (List.ofArray recordFields)
                Symbols (recordFieldSymbols, None)

            // symbolize Union
            elif FSharpType.IsUnion sourceType then
                let (unionCase, unionFields) = FSharpValue.GetUnionFields (source, sourceType)
                let unionFieldTypes = unionCase.GetFields ()
                if not ^ Array.isEmpty unionFields then
                    let unionFieldSymbols = List.mapi (fun i unionField -> toSymbol unionFieldTypes.[i].PropertyType unionField) (List.ofArray unionFields)
                    let unionSymbols = Atom (unionCase.Name, None) :: unionFieldSymbols
                    Symbols (unionSymbols, None)
                else Atom (unionCase.Name, None)

            // symbolize vanilla .NET type
            else
                let typeConverter = TypeDescriptor.GetConverter sourceType
                if typeConverter.CanConvertTo typeof<Symbol>
                then typeConverter.ConvertTo (source, typeof<Symbol>) :?> Symbol
                else (typeConverter.ConvertTo (source, typeof<string>) :?> string, None) |> Atom

    let toString (sourceType : Type) (source : obj) =
        let symbol = toSymbol sourceType source
        Symbol.toString symbol

    let rec fromSymbol (destType : Type) (symbol : Symbol) =

        // desymbolize .NET primitive
        if destType.IsPrimitive then
            match symbol with
            | Atom (str, _) | Number (str, _) | String (str, _) ->
                (TypeDescriptor.GetConverter destType).ConvertFromString str
            | Quote (_, optOrigin) | Symbols (_, optOrigin) ->
                failwith ^ "Expected Symbol, Number, or String for conversion to .NET primitive" + Origin.tryPrint optOrigin

        // desymbolize string
        elif destType = typeof<string> then
            match symbol with
            | Atom (str, _) | Number (str, _) | String (str, _) ->
                str :> obj
            | Quote (_, optOrigin) | Symbols (_, optOrigin) ->
                failwith ^ "Expected Symbol, Number, or String for conversion to string" + Origin.tryPrint optOrigin

        // desymbolize Symbol (no tranformation)
        elif destType = typeof<Symbol> then
            symbol :> obj

        else
            match destType.TryGetCustomTypeConverter () with
            | Some typeConverter ->

                // desymbolize user-defined type
                if typeConverter.CanConvertFrom typeof<Symbol>
                then typeConverter.ConvertFrom symbol
                else failwith ^ "Expected ability to convert from Symbol for custom type converter '" + getTypeName typeConverter + "'."

            | None ->

                // desymbolize array
                if destType.Name = typedefof<_ array>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.ArrayModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|elements|])|])
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to array" + Origin.tryPrint optOrigin

                // desymbolize list
                elif destType.Name = typedefof<_ list>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.ListModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|elements|])|])
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to list" + Origin.tryPrint optOrigin

                // desymbolize Set
                elif destType.Name = typedefof<_ Set>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        let elementType = gargs.[0]
                        let elements = List.map (fromSymbol elementType) symbols
                        let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.SetModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|elementType|]
                        ofSeq.Invoke (null, [|cast.Invoke (null, [|elements|])|])
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to Set" + Origin.tryPrint optOrigin

                // desymbolize Map
                elif destType.Name = typedefof<Map<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairList = List.map (fromSymbol pairType) symbols
                            let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                            let ofSeq = ((FSharpCoreAssembly.GetType "Microsoft.FSharp.Collections.MapModule").GetMethod ("OfSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                            ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                        | _ -> failwithumf ()
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to Map" + Origin.tryPrint optOrigin

                // desymbolize Vmap
                elif destType.Name = typedefof<Vmap<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let gargs = destType.GetGenericArguments ()
                        match gargs with
                        | [|fstType; sndType|] ->
                            let pairType = typedefof<Tuple<_, _>>.MakeGenericType [|fstType; sndType|]
                            let pairList = List.map (fromSymbol pairType) symbols
                            let cast = (typeof<System.Linq.Enumerable>.GetMethod ("Cast", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|pairType|]
                            let ofSeq = ((typedefof<Vmap<_, _>>.Assembly.GetType "Prime.VmapModule+Vmap").GetMethod ("ofSeq", BindingFlags.Static ||| BindingFlags.Public)).MakeGenericMethod [|fstType; sndType|]
                            ofSeq.Invoke (null, [|cast.Invoke (null, [|pairList|])|])
                        | _ -> failwithumf ()
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to Vmap" + Origin.tryPrint optOrigin
                    
                // desymbolize SymbolicCompression
                elif destType.Name = typedefof<SymbolicCompression<_, _>>.Name then
                    match symbol with
                    | Symbols (symbols, _) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: _ ->
                            let gargs = destType.GetGenericArguments ()
                            let aType = gargs.[0]
                            let aCases = FSharpType.GetUnionCases aType
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = symbolHead) aCases with
                            | Some aCase ->
                                let a = fromSymbol aCase.DeclaringType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[0]
                                FSharpValue.MakeUnion (compressionUnion, [|a|])
                            | None ->
                                let bType = gargs.[1]
                                let b = fromSymbol bType symbol
                                let compressionUnion = (FSharpType.GetUnionCases destType).[1]
                                FSharpValue.MakeUnion (compressionUnion, [|b|])
                        | _ -> failwith "Expected Atom value for SymbolicCompression union name."
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to SymbolicCompression" + Origin.tryPrint optOrigin

                // desymbolize Tuple
                elif FSharpType.IsTuple destType then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let elementTypes = FSharpType.GetTupleElements destType
                        let elements = List.mapi (fun i elementSymbol -> fromSymbol elementTypes.[i] elementSymbol) symbols
                        let elements = padWithDefaults' elementTypes elements
                        FSharpValue.MakeTuple (elements, destType)
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to Tuple" + Origin.tryPrint optOrigin

                // desymbolize Record
                elif FSharpType.IsRecord destType then
                    match symbol with
                    | Symbols (symbols, _) ->
                        let fieldTypes = FSharpType.GetRecordFields destType
                        let fields = List.mapi (fun i fieldSymbol -> fromSymbol fieldTypes.[i].PropertyType fieldSymbol) symbols
                        let fields = padWithDefaults fieldTypes fields
                        FSharpValue.MakeRecord (destType, fields)
                    | Atom (_, optOrigin) | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Symbols for conversion to Record" + Origin.tryPrint optOrigin

                // desymbolize Union
                elif FSharpType.IsUnion destType && destType <> typeof<string list> then
                    let unionCases = FSharpType.GetUnionCases destType
                    match symbol with
                    | Atom (unionName, _) ->
                        match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                        | Some unionCase -> FSharpValue.MakeUnion (unionCase, [||])
                        | None ->
                            let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> curry String.Join " | "
                            let optOrigin = Symbol.getOptOrigin symbol
                            failwith ^ "Expected one of the following Atom values for Union name: '" + unionNames + "'" + Origin.tryPrint optOrigin
                    | Symbols (symbols, optOrigin) ->
                        match symbols with
                        | (Atom (symbolHead, _)) :: symbolTail ->
                            let unionName = symbolHead
                            match Array.tryFind (fun (unionCase : UnionCaseInfo) -> unionCase.Name = unionName) unionCases with
                            | Some unionCase ->
                                let unionFieldTypes = unionCase.GetFields ()
                                let unionValues = List.mapi (fun i unionSymbol -> fromSymbol unionFieldTypes.[i].PropertyType unionSymbol) symbolTail
                                let unionValues = padWithDefaults unionFieldTypes unionValues
                                FSharpValue.MakeUnion (unionCase, unionValues)
                            | None ->
                                let unionNames = unionCases |> Array.map (fun unionCase -> unionCase.Name) |> curry String.Join " | "
                                let optOrigin = Symbol.getOptOrigin symbol
                                failwith ^ "Expected one of the following Atom values for Union name: '" + unionNames + "'" + Origin.tryPrint optOrigin
                        | (Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) | Symbols (_, optOrigin)) :: _ ->
                            failwith ^ "Expected Atom value for Union name" + Origin.tryPrint optOrigin
                        | [] -> failwith ^ "Expected Atom value for Union name" + Origin.tryPrint optOrigin
                    | Number (_, optOrigin) | String (_, optOrigin) | Quote (_, optOrigin) ->
                        failwith ^ "Expected Atom or Symbols value for conversion to Union" + Origin.tryPrint optOrigin

                // desymbolize vanilla .NET type
                else
                    match symbol with
                    | Atom (str, _) | Number (str, _) | String (str, _) ->
                        (TypeDescriptor.GetConverter destType).ConvertFromString str
                    | Quote (_, optOrigin) | Symbols (_, optOrigin) ->
                        failwith ^ "Expected Atom, Number, or String value for conversion to vanilla .NET type '" + destType.Name + "'" + Origin.tryPrint optOrigin

    let fromString (destType : Type) (source : string) =
        let symbol = Symbol.fromString source
        fromSymbol destType symbol

    override this.CanConvertTo (_, destType) =
        destType = typeof<string> ||
        destType = typeof<Symbol> ||
        destType = pointType

    override this.ConvertTo (_, _, source, destType) =
        if destType = typeof<string> then
            match source with
            | null ->
                if FSharpType.IsUnion pointType
                then (FSharpType.GetUnionCases pointType).[0].Name :> obj
                else toString pointType source :> obj
            | _ -> toString pointType source :> obj
        elif destType = typeof<Symbol> then toSymbol pointType source :> obj
        elif destType = pointType then source
        else failwith "Invalid SymbolicConverter conversion to source."

    override this.CanConvertFrom (_, sourceType) =
        sourceType = typeof<string> ||
        sourceType = typeof<Symbol> ||
        sourceType = pointType

    override this.ConvertFrom (_, _, source) =
        match source with
        | null -> source
        | _ ->
            let sourceType = source.GetType ()
            if sourceType <> pointType then
                match source with
                | :? string as sourceStr -> fromString pointType sourceStr
                | :? Symbol as sourceSymbol -> fromSymbol pointType sourceSymbol
                | _ -> failwith "Invalid SymbolicConverter conversion from string."
            else source