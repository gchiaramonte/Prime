﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type Origin =
    { Start : Position
      Stop : Position }

    static member printStart origin =
        "[Ln: " + string origin.Start.Line + ", Col: " + string origin.Start.Column + "]"

    static member printStop origin =
        "[Ln: " + string origin.Stop.Line + ", Col: " + string origin.Stop.Column + "]"

    static member print origin =
        " starting at " + Origin.printStart origin + " and stopping at " + Origin.printStop origin + "."

    static member tryPrint optOrigin =
        match optOrigin with
        | Some origin -> Origin.print origin
        | None -> "."

type Symbol =
    | Atom of string * Origin option
    | Number of string * Origin option
    | String of string * Origin option
    | Quote of string * Origin option
    | Symbols of Symbol list * Origin option

[<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Symbol =

    let [<Literal>] NumberFormat =
        NumberLiteralOptions.AllowMinusSign |||
        NumberLiteralOptions.AllowPlusSign |||
        NumberLiteralOptions.AllowExponent |||
        NumberLiteralOptions.AllowFraction |||
        NumberLiteralOptions.AllowHexadecimal

    let [<Literal>] NewlineChars = "\n\r"
    let [<Literal>] WhitespaceChars = " \t" + NewlineChars
    let [<Literal>] SeparatorChar = ' '
    let [<Literal>] SeparatorStr = " "
    let [<Literal>] OpenSymbolsChar = '['
    let [<Literal>] OpenSymbolsStr = "["
    let [<Literal>] CloseSymbolsChar = ']'
    let [<Literal>] CloseSymbolsStr = "]"
    let [<Literal>] OpenStringChar = '\"'
    let [<Literal>] OpenStringStr = "\""
    let [<Literal>] CloseStringChar = '\"'
    let [<Literal>] CloseStringStr = "\""
    let [<Literal>] OpenQuoteChar = '`'
    let [<Literal>] OpenQuoteStr = "`"
    let [<Literal>] CloseQuoteChar = '\''
    let [<Literal>] CloseQuoteStr = "\'"
    let [<Literal>] StructureCharsNoStr = "[]`\'"
    let [<Literal>] StructureChars = "\"" + StructureCharsNoStr
    let [<Literal>] OpsReserved = "@#." // @ reserved for wildcards, # reserved for comment syntax, . reserved for pair syntax (expands to [key value])
    let (*Literal*) OpsUnexpanded = "!$%^&*+-=|<>?/" |> Array.ofSeq
    let (*Literal*) OpsExpanded = [|"Not_"; "At_"; "Num_"; "Set_"; "Mod_"; "Xor_"; "And_"; "Mul_"; "Add_"; "Sub_"; "Eq_"; "Or_"; "Lt_"; "Gt_"; "Get_"; "Div_"|]

    let expand (unexpanded : string) =
        if unexpanded.IndexOfAny OpsUnexpanded = 0 then
            Seq.fold
                (fun expanded chr ->
                    let str =
                        match chr with
                        | '!' -> "Not_"
                        | '$' -> "Set_"
                        | '%' -> "Mod_"
                        | '^' -> "Xor_"
                        | '&' -> "And_"
                        | '*' -> "Mul_"
                        | '+' -> "Add_"
                        | '-' -> "Sub_"
                        | '=' -> "Eq_"
                        | '|' -> "Or_"
                        | '<' -> "Lt_"
                        | '>' -> "Gt_"
                        | '?' -> "Get_"
                        | '/' -> "Div_"
                        | _ -> string chr
                    expanded + str)
                "" unexpanded
        else unexpanded

    let unexpand (expanded : string) =
        if Seq.exists (fun expandedOp -> expanded.StartsWith expandedOp) OpsExpanded then
            expanded
                .Replace("Not_", "!")
                .Replace("Set_", "$")
                .Replace("Mod_", "%")
                .Replace("Xor_", "^")
                .Replace("And_", "&")
                .Replace("Mul_", "*")
                .Replace("Add_", "+")
                .Replace("Sub_", "-")
                .Replace("Eq_",  "=")
                .Replace("Or_",  "|")
                .Replace("Lt_",  "<")
                .Replace("Gt_",  ">")
                .Replace("Get_", "?")
                .Replace("Div_", "/")
        else expanded


    let isExplicit (str : string) =
        str.StartsWith OpenStringStr && str.EndsWith CloseStringStr
    
    let shouldBeExplicit (str : string) =
        Seq.exists (fun chr -> Char.IsWhiteSpace chr || Seq.contains chr StructureCharsNoStr) str

    let skipWhitespace = skipAnyOf WhitespaceChars
    let skipWhitespaces = skipMany skipWhitespace

    let charForm character = skipChar character >>. skipWhitespaces
    let openSymbolsForm = charForm OpenSymbolsChar
    let closeSymbolsForm = charForm CloseSymbolsChar
    let openStringForm = skipChar OpenStringChar
    let closeStringForm = charForm CloseStringChar
    let openQuoteForm = charForm OpenQuoteChar
    let closeQuoteForm = charForm CloseQuoteChar

    let readAtomChars = many1 (noneOf (StructureChars + WhitespaceChars))
    let readStringChars = many (noneOf [CloseStringChar])
    let readQuoteChars = many (noneOf [CloseQuoteChar])
    let readContentChars =
        many1
            ((attempt (openQuoteForm >>. readQuoteChars .>> closeQuoteForm)) <|>
             (attempt (openStringForm >>. readStringChars .>> closeStringForm)) <|>
             (readAtomChars .>> skipWhitespaces))

    let (readSymbol : Parser<Symbol, unit>, private refReadSymbol : Parser<Symbol, unit> ref) =
        createParserForwardedToRef ()

    let readAtom =
        parse {
            let! start = getPosition
            let! chars = readAtomChars
            do! skipWhitespaces
            let! stop = getPosition
            let str = chars |> String.implode |> (fun str -> str.TrimEnd ()) |> expand
            let origin = Some { Start = start; Stop = stop }
            return Atom (str, origin) }

    let readNumber =
        parse {
            let! start = getPosition
            let! number = numberLiteral NumberFormat "number"
            do! skipWhitespaces
            let! stop = getPosition
            let origin = Some { Start = start; Stop = stop }
            return Number (number.String, origin) }

    let readString =
        parse {
            let! start = getPosition
            do! openStringForm
            let! escaped = readStringChars
            do! closeStringForm
            let! stop = getPosition
            let str = escaped |> String.implode
            let origin = Some { Start = start; Stop = stop }
            return String (str, origin) }

    let readQuote =
        parse {
            let! start = getPosition
            do! openQuoteForm
            let! quoteChars = readQuoteChars
            do! closeQuoteForm
            let! stop = getPosition
            let str = quoteChars |> String.implode
            let origin = Some { Start = start; Stop = stop }
            return Quote (str, origin) }

    let readSymbols =
        parse {
            let! start = getPosition
            do! openSymbolsForm
            let! symbols = many readSymbol
            do! closeSymbolsForm
            let! stop = getPosition
            let origin = Some { Start = start; Stop = stop }
            return Symbols (symbols, origin) }

    do refReadSymbol :=
        attempt readQuote <|>
        attempt readString <|>
        attempt readNumber <|>
        attempt readAtom <|>
        readSymbols

    let rec writeSymbol symbol =
        match symbol with
        | Atom (str, _) ->
            if Seq.isEmpty str then OpenStringStr + CloseStringStr
            elif not (isExplicit str) && shouldBeExplicit str then OpenStringStr + str + CloseStringStr
            elif isExplicit str && not (shouldBeExplicit str) then str.Substring (1, str.Length - 2)
            else unexpand str
        | Number (str, _) -> str
        | String (str, _) -> OpenStringStr + str + CloseStringStr
        | Quote (str, _) -> OpenQuoteStr + str + CloseQuoteStr
        | Symbols (symbols, _) -> OpenSymbolsStr + String.Join (" ", List.map writeSymbol symbols) + CloseSymbolsStr

    /// Convert a string to a symbol, with the following parses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]'
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let fromString str =
        match run (skipWhitespaces >>. readSymbol) str with
        | Success (value, _, _) -> value
        | Failure (error, _, _) -> failwith error

    /// Convert a symbol to a string, with the following unparses:
    /// 
    /// (* Atom values *)
    /// None
    /// CharacterAnimationFacing
    /// 
    /// (* Number values *)
    /// 0
    /// -5
    ///
    /// (* String value *)
    /// "String with quoted spaces."
    ///
    /// (* Quoted value *)
    /// `[Some 1]'
    /// 
    /// (* Symbols values *)
    /// []
    /// [Some 0]
    /// [Left 0]
    /// [[0 1] [2 4]]
    /// [AnimationData 4 8]
    /// [Gem `[Some 1]']
    ///
    /// ...and so on.
    let rec toString symbol = writeSymbol symbol