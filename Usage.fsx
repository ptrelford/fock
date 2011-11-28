#load "Fock.fs"
open Fock

type IFock =
    abstract Insert : double * unit -> int

let stub = 
    Stub<IFock>()
        .Method(fun x -> <@ x.Insert(any(),any()) @>).Returns(2)
let instance = stub.Create()
let returnValue = instance.Insert(2.0,())