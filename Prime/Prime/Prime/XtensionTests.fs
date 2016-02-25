﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open System.IO
open System.Xml
open Xunit
open Prime

type [<CLIMutable; NoEquality; NoComparison>] TestXtended =
    { Xtension : Xtension }

    static member (?) (this : TestXtended, memberName) =
        Xtension.(?) (this.Xtension, memberName)

    static member (?<-) (this : TestXtended, memberName, value) =
        let xtension = Xtension.(?<-) (this.Xtension, memberName, value)
        { this with Xtension = xtension }

module XtensionTests =

    let [<Fact>] canAddField () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 5
        let fieldValue = xtn?TestField
        Assert.Equal (5, fieldValue)

    let [<Fact>] cantAddFieldWhenSealed () =
        let xtn = Xtension.safe
        Assert.Throws<Exception> (fun () -> (xtn?TestField <- 0) |> ignore)

    let [<Fact>] cantAccessNonexistentField () =
        let xtn = Xtension.mixed
        let xtn = xtn?TestField <- 5
        Assert.Throws<Exception> (fun () -> xtn?TetField |> ignore)

    let [<Fact>] missingFieldReturnsDefault () =
        let xtn = Xtension.empty
        let xtn = xtn?TestField <- 0
        let fieldValue = xtn?MissingField
        Assert.Equal (0, fieldValue)

    let [<Fact>] canAddFieldViaContainingType () =
        let xtd = { Xtension = Xtension.empty }
        let xtd = xtd?TestField <- 5
        let fieldValue = xtd?TestField
        Assert.Equal (5, fieldValue)