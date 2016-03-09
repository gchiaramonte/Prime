namespace Prime
open System
open System.Text.RegularExpressions

type EventFilter = Regex list

type EventFilters = EventFilter list

[<AutoOpen>]
module EventRecord =

    type [<ReferenceEquality>] EventRecord =
        private
            { ModuleName : string
              FunctionName : string
              Parameters : obj list }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EventRecord =
    
        let record moduleName functionName =
            { ModuleName = moduleName
              FunctionName = functionName
              Parameters = [] }
    
        let record4 moduleName functionName parameters ignoreParameters =
            { ModuleName = moduleName
              FunctionName = functionName
              Parameters = if ignoreParameters then [] else parameters }

[<AutoOpen>]
module EventTrace =

    type EventTrace = EventRecord list
    
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EventTrace =
    
        let filter eventFilter eventTrace =
            List.fold2
                (fun passed (regex : Regex) (record : EventRecord) -> passed && regex.IsMatch ^ scstring record)
                true
                eventFilter
                eventTrace