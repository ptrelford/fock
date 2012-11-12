#load "Fock.fs"

open Fock

let inline Assert success = if not success then failwith "Failed"

module ``Method Example`` =
    let instance =
        Stub<System.Collections.IList>()
            .Method(fun x -> <@ x.Contains(any()) @>).Returns(true)
            .Create()
    Assert(instance.Contains(null))

module ``Method Matching Example`` =
    let instance =
        Stub<System.Collections.IList>()
            .Method(fun x -> <@ x.Contains(1) @>).Returns(true)
            .Method(fun x -> <@ x.Contains(2) @>).Returns(false)
            .Create()
    Assert(instance.Contains(1) = true)
    Assert(instance.Contains(2) = false)

module ``Property Example`` =
    let instance =
        Stub<System.Collections.IList>()
            .Method(fun x -> <@ x.Count @>).Returns(1)
            .Create()
    Assert(instance.Count = 1)

module ``Item Example`` =
    let instance =
        Stub<System.Collections.Generic.IList<double>>()
            .Method(fun x -> <@ x.Item(0) @>).Returns(0.0)
            .Method(fun x -> <@ x.Item(any()) @>).Returns(-1.0)
            .Create()
    Assert(instance.[0] = 0.0)
    Assert(instance.[1] = -1.)

module ``Raise Example`` =
    let instance =
        Stub<System.IComparable>()
            .Method(fun x -> <@ x.CompareTo(any()) @>).Raises<System.ApplicationException>()
            .Create()
    try instance.CompareTo(1) |> ignore; false with e -> true
    |> Assert

module ``Calculator Example`` =
    type ICalculator =
        abstract Push : int -> unit
        abstract Sum : unit -> unit
        abstract Total : int

    let stub = 
        Stub<ICalculator>()
            .Method(fun x -> <@ x.Push(any()) @>).Returns(())
            .Method(fun x -> <@ x.Total @>).Returns(2)
    let instance = stub.Create()
    let returnValue = instance.Push(2)
    Assert(instance.Total = 2)