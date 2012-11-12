module Fock.Tests

open Fock
open NUnit.Framework

type IInterface =
    abstract MethodReturnsSomething : unit -> int
    abstract MethodReturnsNothing : unit -> unit
    abstract Arity1Method : int -> bool
    abstract Arity2Method : int * string -> bool
    abstract StringProperty : string

[<Test>]
let ``an interface method that is not implemented that returns something should throw`` () =
    let stub = Stub<IInterface>().Create()
    Assert.Throws<System.NotImplementedException>( fun () -> 
        stub.MethodReturnsSomething() |> ignore ) |> ignore

[<Test>]
let ``an interface method that is not implemented and returns nothing should not throw`` () =
    let stub = Stub<IInterface>().Create()
    Assert.DoesNotThrow( fun () -> stub.MethodReturnsNothing() )

[<Test>]
let ``an implemented interface method should return the specified value`` () =
    let stub = 
        Stub<IInterface>()
            .Method(fun x -> <@ x.MethodReturnsSomething() @>).Returns(2)
            .Create()
    let returnValue = stub.MethodReturnsSomething()
    Assert.AreEqual(returnValue,2)

[<Test>]
let ``an implemented interface property getter should return the specified value`` () =
    let stub = 
        Stub<IInterface>()
            .Method(fun x -> <@ x.StringProperty @>).Returns("Fock")
            .Create()
    let returnValue = stub.StringProperty
    Assert.AreEqual(returnValue,"Fock")

[<Test>]
let ``an implemented interface method with arity/1 should accept any arguments`` 
    ([<Values(-1,0,9)>] n) =
    let stub =
        Stub<IInterface>()
            .Method(fun x -> <@ x.Arity1Method(any()) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity1Method(n))

[<Test>]
let ``an implemented interface method with arity/2 should accept any arguments`` 
    ([<Values(9,0,-1)>] n) =
    let stub =
        Stub<IInterface>()
            .Method(fun x -> <@ x.Arity2Method(any(),any()) @>).Returns(true)
            .Create()
    Assert.AreEqual(true, stub.Arity2Method(n,"string"))