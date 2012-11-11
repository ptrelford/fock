namespace Fock

open System
open System.Reflection
open System.Reflection.Emit

module internal CodeEmit =
    type Arg = Any | Arg of obj
    type Result = ReturnValue of obj | Throw of Type

    let generateConstructor (typeBuilder:TypeBuilder) ps (genBody:ILGenerator -> unit) =
        let cons = 
           typeBuilder.DefineConstructor(
                MethodAttributes.Public,
                CallingConventions.Standard,
                ps)  
        let gen = cons.GetILGenerator()
        // Call base constructor
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
        // Generate body
        genBody gen
        gen.Emit(OpCodes.Ret)

    let defineMethod (typeBuilder:TypeBuilder) (abstractMethod:MethodInfo) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        let args = abstractMethod.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
        typeBuilder.DefineMethod(
            abstractMethod.Name, 
            attr,
            abstractMethod.ReturnType, 
            args)

    let stub<'TAbstract when 'TAbstract : not struct> 
        (calls:(MethodInfo * (Arg [] * Result)) list) =
        
        let abstractType = typeof<'TAbstract>
        let stubName = "Stub" + abstractType.Name
        let assemblyBuilder =
            AppDomain
                .CurrentDomain
                .DefineDynamicAssembly(AssemblyName(stubName),AssemblyBuilderAccess.Run)
    
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(stubName+".dll")

        let typeBuilder = 
            let parent, interfaces = 
                if abstractType.IsInterface 
                then typeof<obj>, [|abstractType|]
                else typeof<'TAbstract>, [||]
            moduleBuilder.DefineType(
                stubName,
                TypeAttributes.Public ||| TypeAttributes.Class,
                parent,
                interfaces)
    
        let returnValuesField =
            typeBuilder.DefineField(
                "_returnValues",
                typeof<obj[]>,
                FieldAttributes.Private ||| FieldAttributes.InitOnly)

        let argsField =
            typeBuilder.DefineField(
                "_args",
                typeof<obj[][]>,
                FieldAttributes.Private ||| FieldAttributes.InitOnly)

        // Generate default constructor
        generateConstructor typeBuilder [||] (fun _ -> ())
    
        // Generate constructor overload
        let setFields (gen:ILGenerator) =
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Stfld, returnValuesField)
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_2)
            gen.Emit(OpCodes.Stfld, argsField)

        generateConstructor typeBuilder [|typeof<obj[]>;typeof<obj[][]>|] setFields
       
        let groupedMethods = calls |> Seq.groupBy fst
        let argsLookup = ResizeArray<obj[]>()
        let returnValues = ResizeArray<obj>()

        // Implement type's methods
        for abstractMethod in abstractType.GetMethods() do
            let methodBuilder = defineMethod typeBuilder abstractMethod
            let gen = methodBuilder.GetILGenerator()
            let overloads = groupedMethods |> Seq.tryFind (fst >> (=) abstractMethod)
            match overloads with
            | Some (_, overloads) ->
                let _,(args,result) = overloads |> Seq.head
                match result with
                | ReturnValue(value) ->
                    let index = returnValues.Count
                    returnValues.Add(value)
                    gen.Emit(OpCodes.Ldarg_0)
                    gen.Emit(OpCodes.Ldfld, returnValuesField)
                    gen.Emit(OpCodes.Ldc_I4, index)
                    gen.Emit(OpCodes.Ldelem_Ref)
                    gen.Emit(OpCodes.Unbox_Any, abstractMethod.ReturnType)
                    gen.Emit(OpCodes.Ret)
                | Throw(excType) -> 
                    gen.ThrowException(excType)
            | None ->
                if abstractMethod.ReturnType = typeof<System.Void> then
                    gen.Emit(OpCodes.Ret)
                else    
                    gen.ThrowException(typeof<NotImplementedException>)
            if abstractType.IsInterface then 
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)

        let stubType = typeBuilder.CreateType()
        let generatedObject = Activator.CreateInstance(stubType, [|box (returnValues.ToArray());box (argsLookup.ToArray())|])
        generatedObject :?> 'TAbstract

open CodeEmit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

[<AttributeUsage(AttributeTargets.Method)>]
type WildcardAttribute() =
    inherit Attribute()

type Stub<'TAbstract when 'TAbstract : not struct> 
    internal (calls) =
    let t = typeof<'TAbstract>
    let toArgs args =
        let isWildcard (mi:MethodInfo) = 
            mi.GetCustomAttributes(typeof<WildcardAttribute>, true).Length > 0
        [|for arg in args ->
            match arg with
            | Value(v,t) -> Arg(v)
            | Call(_,mi, _) when isWildcard mi -> Any
            | _ -> raise <| NotSupportedException()
        |]
    let toCall = function
        | Call(Some(x), mi, args) when x.Type = t -> mi, toArgs args
        | PropertyGet(Some(x), pi, args) when x.Type = t -> pi.GetGetMethod(), toArgs args
        | _ -> raise <| NotSupportedException()
    new () = Stub([])
    member this.Member(f:'TAbstract -> Expr<'TReturnValue>) = this.Method(f)
    member this.Method(f:'TAbstract -> Expr<'TReturnValue>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let call = toCall (f default')
        MethodBuilder<'TAbstract,'TReturnValue>(call,calls)
    member this.Create() = stub<'TAbstract>(calls)
and MethodBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> 
    internal (call, calls) =
    member this.Returns(value:'TReturnValue) =
        let mi, args = call
        Stub<'TAbstract>((mi, (args, ReturnValue(box value)))::calls)
    [<RequiresExplicitTypeArguments>]
    member this.Throws<'TException when 'TException : ( new : unit -> 'TException ) and 'TException :> exn>() =
        let mi, args = call
        Stub<'TAbstract>((mi, (args, Throw(typeof<'TException>)))::calls)

[<Sealed>]
type It private () =
    [<Wildcard>] static member IsAny<'a>() = Unchecked.defaultof<'a>

[<AutoOpen>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module It =
    let [<Wildcard>] inline any () : 'a = It.IsAny()

module Test =
    type IFock =
        abstract Insert : double * unit -> int
        abstract DoNothing : unit -> unit

    let stub = 
        Stub<IFock>()
            .Method(fun x -> <@ x.Insert(any(),any()) @>).Returns(2)
            .Method(fun x -> <@ x.DoNothing() @>).Throws<ApplicationException>()
    let instance = stub.Create()
    let returnValue = instance.Insert(2.0,())
    do instance.DoNothing()
