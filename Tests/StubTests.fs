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

[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    // These properties are not declared abstract. They 
    // cannot be overriden. 
    member this.CenterX with get() = x and set xval = x <- xval
    member this.CenterY with get() = y and set yval = y <- yval

    // These properties are abstract, and no default implementation 
    // is provided. Non-abstract derived classes must implement these. abstract Area : floatwith get
    abstract Perimeter : float with get
    abstract Name : string with get

    // This method is not declared abstract. It cannot be 
    // overriden. 
    member this.Move dx dy =
       x <- x + dx
       y <- y + dy

    // An abstract method that is given a default implementation 
    // is equivalent to a virtual method in other .NET languages. 
    // Rotate changes the internal angle of rotation of the square. 
    // Angle is assumed to be in degrees. 
    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle

[<Test>]
let ``an implemented abstract class property should return the specified value`` () =
    let stub =
        Stub<Shape2D>()
            .Method(fun x -> <@ x.Name @>).Returns("Name")
            .Create()
    Assert.AreEqual("Name", stub.Name)

[<AbstractClass>]
type AbstractBaseClass() =
   // abstract method
   abstract member Add: int * int -> int

   // abstract immutable property
   abstract member Pi : float 

   // abstract read/write property
   abstract member Area : float with get,set

[<Test>]
let ``an implemented abstract base class method should return the specified value`` () =
    let stub =
        Stub<AbstractBaseClass>()
            .Method(fun x -> <@ x.Add(any(),any()) @>).Returns(2)
            .Create()
    Assert.AreEqual(stub.Add(1,1), 2)
