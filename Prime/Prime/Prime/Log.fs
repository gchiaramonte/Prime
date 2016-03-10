﻿// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.Diagnostics

[<RequireQualifiedAccess>]
module Log =

    let mutable private Initialized = false

    let getUtcNowStr () =
        let now = DateTime.UtcNow
        now.ToString "yyyy-MM-dd HH\:mm\:ss.ffff"

    /// Log a remark message with a Trace.WriteLine.
    let remark message =
        Trace.WriteLine (getUtcNowStr () + "|Remark|" + message)

    /// Log a debug message with Debug.Fail and call to remark.
    let debug message =
#if DEBUG
        Debug.Fail (getUtcNowStr () + "|Debug|" + message)
#else
        ignore message
#endif

    /// Conditional debug message call where condition is lazily evaluated.
    let debugIf predicate message =
#if DEBUG
        if predicate () then debug message
#else
        ignore (predicate, message)
#endif

    /// Log a trace message with a Trace.Fail and call to remark.
    let trace message =
        Trace.Fail (getUtcNowStr () + "|Trace|" + message)

    /// Conditional trace message call where condition is eagerly evaluted.
    let traceIf bl message =
        if bl then trace message

    /// Initialize logging.
    let init (optFileName : string option) =

        // init only once
        if not Initialized then

            // add listeners
            let listeners =
#if DEBUG
                Debug.Listeners
#else
                Trace.Listeners
#endif
            listeners.Add (new TextWriterTraceListener (Console.Out)) |> ignore
            match optFileName with
            | Some fileName -> listeners.Add (new TextWriterTraceListener (fileName)) |> ignore
            | None -> ()

            // automatically flush all logs
            Debug.AutoFlush <- true
            Trace.AutoFlush <- true

            // mark as Initialized
            Initialized <- true