﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime.Tests
open System
open Xunit
open Prime
module RandTests =

    let [<Literal>] Samples = 32768

    let makeSamples (next : Rand -> ('n * Rand)) =
        let refRand = ref ^ Rand.make ()
        [for _ in 0 .. Samples - 1 do
            let (n, r) = next !refRand
            refRand := r
            yield n]

    let [<Fact>] nextDoubleIsInRange () =
        let samples = makeSamples (fun rand -> Rand.nextDouble rand)
        let avg = List.average samples
        Assert.InRange (avg, 0.49, 0.51)

    let [<Fact>] nextSingleIsInRange () =
        let samples = makeSamples (fun rand -> Rand.nextSingle rand)
        let avg = List.average samples
        Assert.InRange (avg, 0.49f, 0.51f)

    let [<Fact>] nextIntIsInRange () =
        let samples = makeSamples (fun rand -> Rand.nextInt rand)
        let sampleDoubles = List.map double samples
        let avg = List.average sampleDoubles
        Assert.InRange (avg, 1003741823.0, 1143741823.0)