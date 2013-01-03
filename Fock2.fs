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
    let toCall (expr:Expression) =
        match expr with
        | :? MethodCallExpression as call ->
            call.Method, toArgs call.Arguments
        | _ -> raise <| NotSupportedException()
    /// Specifies a member function of the abstract type
    member this.SetupFunc(expr:Expression<Func<'TAbstract,'TReturnValue>>) =
        FuncBuilder<'TAbstract,'TReturnValue>(toCall expr.Body,calls)
    /// Specifies a member action of the abstract type
    member this.SetupAction(expr:Expression<Action<'TAbstract>>) =
        ActionBuilder<'TAbstract>(toCall expr.Body,calls)
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