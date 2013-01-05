namespace Fock2

open System
open System.Linq.Expressions
open System.Reflection
open Fock.CodeEmit

/// Generic stub type over abstract types and interfaces
type Stub<'TAbstract when 'TAbstract : not struct> internal (calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Converts argument expressions to Arg array
    let toArgs (args:Expression seq) =
        [| for arg in args -> Any |]
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toMethodInfo (expr:Expression) =
        match expr with
        | :? MethodCallExpression as call ->
            call.Method, toArgs call.Arguments
        | _ -> raise <| NotSupportedException()
    let toPropertyInfo (expr:Expression) =
        match expr with
        | :? MemberExpression as call ->
            let pi = call.Member :?> PropertyInfo
            pi, [||]
        | :? MethodCallExpression as call ->
            let pi = 
                call.Method.DeclaringType.GetProperties() 
                |> Seq.find (fun pi -> pi.GetGetMethod() = call.Method)
            pi, toArgs call.Arguments
        | _ -> raise <| NotSupportedException()
    /// Specifies a member function of the abstract type
    member this.SetupFunc(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        FuncBuilder<'TAbstract,'TReturnValue>(toMethodInfo expr.Body,calls)
    /// Specifies a member action of the abstract type
    member this.SetupAction(expr:Expression<Action<'TAbstract>>) =
        ActionBuilder<'TAbstract>(toMethodInfo expr.Body,calls)
    /// Specifies a property getter of the abstract type
    member this.SetupPropertyGet(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        let pi, args = toPropertyInfo expr.Body
        let call = pi.GetGetMethod(), args 
        FuncBuilder<'TAbstract, 'TReturnValue>(call,calls)
    /// Specifies a property setter of the abstract type
    member this.SetupPropertySet(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        let pi, args = toPropertyInfo expr.Body
        let call = pi.GetSetMethod(), args
        ActionBuilder<'TAbstract>(call, calls)
    /// Specifies an event of the abstract type as a quotation
    member this.SetupEvent(name:string) =
        let e = abstractType.GetEvent(name)
        let handlers = e.GetAddMethod(), e.GetRemoveMethod()
        EventBuilder<'TAbstract>(handlers,calls)
    /// Constructs mock builder
    new () = Stub([])
    /// Creates a generic instance of the abstract type
    member this.Create() = stub<'TAbstract>(calls)
and ActionBuilder<'TAbstract when 'TAbstract : not struct>
    internal (call, calls) =
    let mi, args = call
    /// Specifies the exception a method or property raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) 
                                   and  'TException :> exn>() =
        Stub<'TAbstract>((mi, (args, Raise(typeof<'TException>)))::calls)
and FuncBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct>
    internal (call, calls) =
    inherit ActionBuilder<'TAbstract>(call, calls)
    let mi, args = call    
    /// Specifies the return value of a method or property
    member this.Returns(value:'TReturnValue) =
        let result = 
            if typeof<'TReturnValue> = typeof<unit> then Unit 
            else ReturnValue(value,typeof<'TReturnValue>)
        Stub<'TAbstract>((mi, (args, result))::calls)
/// Generic builder for specifying event values
and EventBuilder<'TAbstract when 'TAbstract : not struct> 
    internal (handlers, calls) =
    let add, remove = handlers
    /// Specifies the published event value
    member this.Publishes(value:IDelegateEvent<'TDelegate>) =
        Stub<'TAbstract>((add, ([|Any|], Handler("AddHandler",value)))::
                         (remove, ([|Any|], Handler("RemoveHandler",value)))::
                         calls)

[<Sealed>]
type It private () =
    /// Marks argument as matching any value
    [<Fock.Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Fock.Predicate>] static member Is<'TArg>(f:'TArg -> bool) = Unchecked.defaultof<'TArg>