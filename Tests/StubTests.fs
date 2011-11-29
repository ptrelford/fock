module Fock.Tests

open Fock
open NUnit.Framework

type IFock =
    abstract Insert : double * unit -> int
    abstract DoNothing : unit -> unit
    abstract Name : string

[<Test>]
let ``an interface method that is not implemented that returns something should throw`` () =
    let stub = Stub<IFock>().Create()
    Assert.Throws<System.NotImplementedException>( fun () -> stub.Insert(1.0,()) |> ignore ) |> ignore

[<Test>]
let ``an interface method that is not implemented and returns nothing should not throw`` () =
    let stub = Stub<IFock>().Create()
    Assert.DoesNotThrow( fun () -> stub.DoNothing() )

[<Test>]
let ``an implemented interface method should return the specified value`` () =
    let stub = 
        Stub<IFock>()
            .Method(fun x -> <@ x.Insert(any(),any()) @>).Returns(2)
            .Create()
    let returnValue = stub.Insert(2.0,())
    Assert.AreEqual(returnValue,2)

[<Test>]
let ``an implemented interface property getter should return the specified value`` () =
    let stub = 
        Stub<IFock>()
            .Method(fun x -> <@ x.Name @>).Returns("Fock")
            .Create()
    let returnValue = stub.Name    
    Assert.AreEqual(returnValue,"Fock")

