namespace Fock

open System
open System.Reflection
open System.Reflection.Emit

module internal CodeEmit =
    /// Method argument type
    type Arg = Any | Arg of obj
    /// Method result type
    type Result = Unit | ReturnValue of obj | Raise of Type
    /// Generates constructor
    let generateConstructor (typeBuilder:TypeBuilder) ps (genBody:ILGenerator -> unit) =
        let cons = typeBuilder.DefineConstructor(MethodAttributes.Public,CallingConventions.Standard,ps)
        let gen = cons.GetILGenerator()
        // Call base constructor
        gen.Emit(OpCodes.Ldarg_0)
        gen.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
        // Generate body
        genBody gen
        gen.Emit(OpCodes.Ret)
    /// Defines method
    let defineMethod (typeBuilder:TypeBuilder) (abstractMethod:MethodInfo) =
        let attr = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Virtual
        let args = abstractMethod.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
        typeBuilder.DefineMethod(abstractMethod.Name, attr, abstractMethod.ReturnType, args)
    /// Builds a stub from the specified calls
    let stub<'TAbstract when 'TAbstract : not struct> 
        (calls:(MethodInfo * (Arg [] * Result)) list) =
        /// Abstract type
        let abstractType = typeof<'TAbstract>
        /// Stub name for abstract type
        let stubName = "Stub" + abstractType.Name
        /// Builder for assembly
        let assemblyBuilder =
            AppDomain.CurrentDomain.DefineDynamicAssembly(AssemblyName(stubName),AssemblyBuilderAccess.Run)
        /// Builder for module
        let moduleBuilder = assemblyBuilder.DefineDynamicModule(stubName+".dll")
        /// Builder for abstract type
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
        /// Field for method return values
        let returnValuesField =
            typeBuilder.DefineField(
                "_returnValues",
                typeof<obj[]>,
                FieldAttributes.Private ||| FieldAttributes.InitOnly)
        /// Field for method arguments 
        let argsField =
            typeBuilder.DefineField(
                "_args",
                typeof<obj[][]>,
                FieldAttributes.Private ||| FieldAttributes.InitOnly)
        // Generate default constructor
        generateConstructor typeBuilder [||] (fun _ -> ())
        // Set fields from constructor arguments
        let setFields (gen:ILGenerator) =
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_1)
            gen.Emit(OpCodes.Stfld, returnValuesField)
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldarg_2)
            gen.Emit(OpCodes.Stfld, argsField)
        // Generate constructor overload
        generateConstructor typeBuilder [|typeof<obj[]>;typeof<obj[][]>|] setFields
        /// Method overloads grouped by type
        let groupedMethods = calls |> Seq.groupBy fst
        /// Method argument lookup
        let argsLookup = ResizeArray<obj[]>()
        /// Method return values
        let returnValues = ResizeArray<obj>()
        /// Abstract type's methods including interfaces
        let abstractMethods = seq { 
            yield! abstractType.GetMethods()
            for interfaceType in abstractType.GetInterfaces() do
                yield! interfaceType.GetMethods()
            }
        // Implement abstract type's methods
        for abstractMethod in abstractMethods do
            let methodBuilder = defineMethod typeBuilder abstractMethod
            let gen = methodBuilder.GetILGenerator()
            let overloads = groupedMethods |> Seq.tryFind (fst >> (=) abstractMethod)
            match overloads with
            | Some (_, overloads) ->
                overloads |> Seq.iter (fun (_,(args, result)) ->
                    let unmatched = gen.DefineLabel()
                    let argsLookupIndex = argsLookup.Count
                    // Add arguments to lookup
                    args |> Array.map (function Any -> null | Arg(value) -> box value) |> argsLookup.Add
                    // Emit argument matching
                    args |> Seq.iteri (fun argIndex arg ->
                        match arg with
                        | Any -> ()
                        | Arg(value) ->
                            gen.Emit(OpCodes.Ldarg, argIndex+1)
                            gen.Emit(OpCodes.Box, abstractMethod.GetParameters().[argIndex].ParameterType)
                            gen.Emit(OpCodes.Ldarg_0)
                            gen.Emit(OpCodes.Ldfld, argsField)
                            gen.Emit(OpCodes.Ldc_I4, argsLookupIndex)
                            gen.Emit(OpCodes.Ldelem_Ref)
                            gen.Emit(OpCodes.Ldc_I4, argIndex)
                            gen.Emit(OpCodes.Ldelem_Ref)
                            gen.EmitCall(OpCodes.Call, typeof<obj>.GetMethod("Equals",[|typeof<obj>;typeof<obj>|]), null) 
                            gen.Emit(OpCodes.Brfalse_S, unmatched)
                    )
                    // Emit result
                    match result with
                    | Unit -> gen.Emit(OpCodes.Ret)
                    | ReturnValue(value) ->
                        let returnValuesIndex = returnValues.Count
                        returnValues.Add(value)
                        gen.Emit(OpCodes.Ldarg_0)
                        gen.Emit(OpCodes.Ldfld, returnValuesField)
                        gen.Emit(OpCodes.Ldc_I4, returnValuesIndex)
                        gen.Emit(OpCodes.Ldelem_Ref)
                        gen.Emit(OpCodes.Unbox_Any, abstractMethod.ReturnType)
                        gen.Emit(OpCodes.Ret)
                    | Raise(excType) -> gen.ThrowException(excType)
                    gen.MarkLabel(unmatched)
                )
                gen.ThrowException(typeof<MatchFailureException>)
            | None ->
                if abstractMethod.ReturnType = typeof<System.Void> then
                    gen.Emit(OpCodes.Ret)
                else    
                    gen.ThrowException(typeof<NotImplementedException>)
            if abstractType.IsInterface then
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)
        /// Stub type
        let stubType = typeBuilder.CreateType()
        /// Generated object instance
        let generatedObject = 
            Activator.CreateInstance(
                stubType, 
                [|box (returnValues.ToArray());box (argsLookup.ToArray())|])
        generatedObject :?> 'TAbstract

open CodeEmit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Wildcard attribute
[<AttributeUsage(AttributeTargets.Method)>]
type WildcardAttribute() =
    inherit Attribute()

/// Generic stub type over abstract type
type Stub<'TAbstract when 'TAbstract : not struct> internal (calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Converts argument expressions to Arg array
    let toArgs args =
        let isWildcard (mi:MethodInfo) = 
            mi.GetCustomAttributes(typeof<WildcardAttribute>, true).Length > 0
        [|for arg in args ->
            match arg with
            | Value(v,t) -> Arg(v)
            | Call(_,mi, _) when isWildcard mi -> Any
            | _ -> raise <| NotSupportedException()
        |]
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toCall = function
        | Call(Some(x), mi, args) when x.Type = abstractType -> mi, toArgs args
        | PropertyGet(Some(x), pi, args) when x.Type = abstractType -> pi.GetGetMethod(), toArgs args
        | _ -> raise <| NotSupportedException()
    new () = Stub([])
    /// Specifies a method of the abstract type as a quotation
    member this.Method(f:'TAbstract -> Expr<'TReturnValue>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let call = toCall (f default')
        ResultBuilder<'TAbstract,'TReturnValue>(call,calls)
    /// Creates an instance of the abstract type
    member this.Create() = stub<'TAbstract>(calls)
/// Generic builder for specifying method results
and ResultBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> 
    internal (call, calls) =
    /// Specifies the return value of a method
    member this.Returns(value:'TReturnValue) =
        let mi, args = call
        let result = if typeof<'TReturnValue> = typeof<unit> then Unit else ReturnValue(value)
        Stub<'TAbstract>((mi, (args, result))::calls)
    /// Specifies the exception a method raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) and 'TException :> exn>() =
        let mi, args = call
        Stub<'TAbstract>((mi, (args, Raise(typeof<'TException>)))::calls)

[<Sealed>]
type It private () =
    /// Marks argument as accepting any value
    [<Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module It =
    /// Marks argument as accepting any value
    let [<Wildcard>] inline any () : 'TArg = It.IsAny()

module ``Method Example`` =
    let instance =
        Stub<System.Collections.IList>()
            .Method(fun x -> <@ x.Contains(any()) @>).Returns(true)
            .Create()
    System.Diagnostics.Debug.Assert(instance.Contains(null))

module ``Property Example`` =
    let instance =
        Stub<System.Collections.IList>()
            .Method(fun x -> <@ x.Count @>).Returns(1)
            .Create()
    System.Diagnostics.Debug.Assert(instance.Count = 1)

module ``Item Example`` =
    let instance =
        Stub<System.Collections.Generic.IList<double>>()
            .Method(fun x -> <@ x.Item(any()) @>).Returns(1.0)
            .Create()
    System.Diagnostics.Debug.Assert(instance.[0] = 1.0)

module ``Raise Example`` =
    let instance =
        Stub<System.IComparable>()
            .Method(fun x -> <@ x.CompareTo(any()) @>).Raises<ApplicationException>()
            .Create()
    try instance.CompareTo(1) |> ignore with e -> ()

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
    System.Diagnostics.Debug.Assert(instance.Total = 2)