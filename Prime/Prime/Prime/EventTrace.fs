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
              MoreInfo : string }

    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EventRecord =
    
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

    type EventTrace = EventRecord list
    
    [<RequireQualifiedAccess; CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
    module EventTrace =
    
        let filter eventFilter eventTrace =
            List.fold2
                (fun passed (regex : Regex) (record : EventRecord) -> passed && regex.IsMatch ^ scstring record)
                true
                eventFilter
                eventTrace

        let record moduleName functionName eventTrace =
            EventRecord.record moduleName functionName :: eventTrace

        let record4 moduleName functionName moreInfo eventTrace =
            EventRecord.record3 moduleName functionName moreInfo :: eventTrace

        let empty = []