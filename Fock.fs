namespace Fock

open System
open System.Reflection
open System.Reflection.Emit

module internal CodeEmit =
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
        (calls:(MethodInfo * obj) list) =
        
        let abstractType = typeof<'TAbstract>
        let stubName = "Stub" + abstractType.Name
        let assemblyBuilder =
            AppDomain
                .CurrentDomain
                .DefineDynamicAssembly(AssemblyName(stubName),AssemblyBuilderAccess.Run)
    
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(stubName+".dll")

        let parent, interfaces = 
            if abstractType.IsInterface 
            then typeof<obj>, [|abstractType|]
            else typeof<'TAbstract>, [||]
        let typeBuilder = 
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
    
        generateConstructor typeBuilder [||] (fun _ -> ())
    
        let setReturnValuesField (gen:ILGenerator) =
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Stfld, returnValuesField)
        generateConstructor typeBuilder [|typeof<obj[]>|] setReturnValuesField

        let abstractMethods = abstractType.GetMethods()
        for abstractMethod in abstractMethods do
            let index = 
                let ps = abstractMethod.GetParameters()
                calls
                |> List.tryFindIndex (fun (mi, returnValue) ->
                    let ps' = mi.GetParameters()
                    abstractMethod.Name = mi.Name &&
                    ps.Length = ps'.Length &&
                    Array.forall2 (=) ps ps'
                )
            let methodBuilder = defineMethod typeBuilder abstractMethod
            let gen = methodBuilder.GetILGenerator() 
            index |> function
            | Some index ->
                let mi, _ = calls.[index]
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Ldfld, returnValuesField)
                gen.Emit(OpCodes.Ldc_I4, index)
                gen.Emit(OpCodes.Ldelem_Ref)
                gen.Emit(OpCodes.Unbox_Any, mi.ReturnType)
                gen.Emit(OpCodes.Ret)
            | None ->
                let ctor = typeof<NotImplementedException>.GetConstructor([||])
                gen.Emit(OpCodes.Newobj, ctor)
                gen.Emit(OpCodes.Throw)
            if abstractType.IsInterface then 
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)

        let returnValues = calls |> List.map (fun (_,value) -> value) |> List.toArray
        let stubType = typeBuilder.CreateType()
        let generatedObject = Activator.CreateInstance(stubType, box returnValues)
        generatedObject :?> 'TAbstract

open CodeEmit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type Stub<'TAbstract when 'TAbstract : not struct> 
    internal (calls) =
    let t = typeof<'TAbstract>
    let getMethod = function
        | Call(Some(x), mi, args) when x.Type = t -> mi
        | PropertyGet(Some(x), pi, args) when x.Type = t -> pi.GetGetMethod()
        | _ -> invalidOp ""
    new () = Stub([])
    member this.Method(f:'TAbstract -> Expr<'a>) = 
        let default' = Unchecked.defaultof<'TAbstract>
        let mi = getMethod (f default')
        MethodBuilder<'TAbstract>(mi,calls)
    member this.Create() = stub<'TAbstract>(calls)
and MethodBuilder<'TAbstract when 'TAbstract : not struct> 
    internal (mi, calls) =
    member this.Returns(value:'TReturnValue) =
        Stub<'TAbstract>((mi,box value)::calls)

[<AutoOpen>]
module It =
    let IsAny<'a> = obj() :?> 'a
    let inline any () : 'a = IsAny()