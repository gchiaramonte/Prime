// Prime - A PRIMitivEs code library.
// Copyright (C) Bryan Edds, 2012-2016.

namespace Prime
open System
open System.IO
open System.Collections.Generic
open Prime

/// Describes whether an in-flight event has been resolved or should cascade to down-stream handlers.
type Handling =
    | Resolve
    | Cascade

/// Specifies whether an event-based application is running or exiting.
type Liveness =
    | Running
    | Exiting

/// A participant in the event system.
type Participant =
    interface
        abstract member ParticipantAddress : Participant Address
        abstract member GetPublishingPriority : (Participant -> 'w -> single) -> 'w -> single
        end

/// Operators for the Participant type.
type ParticipantOperators =
    private
        | ParticipantOperators

    /// Concatenate two addresses, forcing the type of first address.
    static member acatf<'a> (address : 'a Address) (participant : Participant) = acatf address (atooa participant.ParticipantAddress)

    /// Concatenate two addresses, takings the type of first address.
    static member (->-) (address, participant : Participant) = ParticipantOperators.acatf address participant

/// An event used by the event system.
type [<ReferenceEquality>] Event<'a, 'p when 'p :> Participant> =
    { Data : 'a
      Address : 'a Address
      Trace : EventTrace
      Publisher : Participant
      Subscriber : 'p }

/// Describes an event subscription.
type Subscription<'a, 'p, 'w when 'p :> Participant> =
    Event<'a, 'p> -> 'w -> Handling * 'w

/// An entry in the subscription map.
type SubscriptionEntry =
    Guid * Participant * obj

/// Abstracts over a subscription sorting procedure.
type 'w SubscriptionSorter =
    SubscriptionEntry list -> 'w -> SubscriptionEntry list

/// Describes an event subscription that can be boxed / unboxed.
type internal 'w BoxableSubscription =
    obj -> 'w -> Handling * 'w

/// A map of event subscriptions.
type internal SubscriptionEntries =
    Vmap<obj Address, SubscriptionEntry list>

/// A map of subscription keys to unsubscription data.
type internal UnsubscriptionEntries =
    Vmap<Guid, obj Address * Participant>

/// The data for a change in a participant.
type [<StructuralEquality; NoComparison>] ParticipantChangeData<'p, 'w when 'p :> Participant> =
    { Participant : 'p
      OldWorld : 'w }

[<RequireQualifiedAccess>]
module Events =

    /// Represents any event.
    let Any = ntoa<obj> !!"*"

[<AutoOpen>]
module EventSystemModule =

    /// A publisher-neutral, purely functional event system.
    type [<ReferenceEquality>] 'w EventSystem =
        private
            { Subscriptions : SubscriptionEntries
              Unsubscriptions : UnsubscriptionEntries
              EventStates : Vmap<Guid, obj>
              EventLogger : string -> unit
              EventLogging : bool
              EventFilter : EventFilter }

    [<RequireQualifiedAccess>]
    module EventSystem =

        /// Add event state.
        let addEventState<'a, 'w> key (state : 'a) (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = Vmap.add key (state :> obj) eventSystem.EventStates }

        /// Remove event state.
        let removeEventState<'w> key (eventSystem : 'w EventSystem) =
            { eventSystem with EventStates = Vmap.remove key eventSystem.EventStates }

        /// Get subscriptions.
        let getSubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Subscriptions

        /// Get unsubscriptions.
        let getUnsubscriptions<'w> (eventSystem : 'w EventSystem) =
            eventSystem.Unsubscriptions

        /// Set subscriptions.
        let internal setSubscriptions<'w> subscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Subscriptions = subscriptions }

        /// Set unsubscriptions.
        let internal setUnsubscriptions<'w> unsubscriptions (eventSystem : 'w EventSystem) =
            { eventSystem with Unsubscriptions = unsubscriptions }

        /// Get event state.
        let getEventState<'a, 'w> key (eventSystem : 'w EventSystem) =
            let state = Vmap.find key eventSystem.EventStates
            state :?> 'a

        /// Log an event.
        let logEvent<'w> (address : obj Address) (trace : EventTrace) (eventSystem : 'w EventSystem) =
            if eventSystem.EventLogging then
                let addressStr = scstring address
                let traceRev = List.rev trace // for efficiency during normal execution, trace is cons'd up into a reversed list
                if EventFilter.filter addressStr traceRev eventSystem.EventFilter then
                    eventSystem.EventLogger ^ addressStr + "|" + scstring traceRev

        /// Make an event system.
        let make eventLogger eventLogging eventFilter =
            { Subscriptions = Vmap.makeEmpty ()
              Unsubscriptions = Vmap.makeEmpty ()
              EventStates = Vmap.makeEmpty ()
              EventLogger = eventLogger
              EventLogging = eventLogging
              EventFilter = eventFilter }