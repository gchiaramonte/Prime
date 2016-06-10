// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.ComponentModel
open Microsoft.FSharp.Reflection
open FParsec
open Prime

type Symbol =
    | Atom of string
    | Number of string
    | String of string
    | Quote of string
    | Symbols of Symbol list

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
            let! chars = readAtomChars
            do! skipWhitespaces
            return chars |> String.implode |> (fun str -> str.TrimEnd ()) |> expand |> Atom }

    let readNumber =
        parse {
            let! number = numberLiteral NumberFormat "number"
            do! skipWhitespaces
            return Number number.String }

    let readString =
        parse {
            do! openStringForm
            let! escaped = readStringChars
            do! closeStringForm
            return escaped |> String.implode |> String }

    let readQuote =
        parse {
            do! openQuoteForm
            let! quoteChars = readQuoteChars
            do! closeQuoteForm
            return quoteChars |> String.implode |> Quote }

    let readSymbols =
        parse {
            do! openSymbolsForm
            let! symbols = many readSymbol
            do! closeSymbolsForm
            return symbols |> Symbols }

    do refReadSymbol :=
        attempt readQuote <|>
        attempt readString <|>
        attempt readNumber <|>
        attempt readAtom <|>
        readSymbols

    let rec writeSymbol symbol =
        match symbol with
        | Atom str ->
            if Seq.isEmpty str then OpenStringStr + CloseStringStr
            elif not (isExplicit str) && shouldBeExplicit str then OpenStringStr + str + CloseStringStr
            elif isExplicit str && not (shouldBeExplicit str) then str.Substring (1, str.Length - 2)
            else unexpand str
        | Number str -> str
        | String str -> OpenStringStr + str + CloseStringStr
        | Quote str -> OpenQuoteStr + str + CloseQuoteStr
        | Symbols symbols -> OpenSymbolsStr + String.Join (" ", List.map writeSymbol symbols) + CloseSymbolsStr

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