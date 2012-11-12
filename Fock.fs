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
    let stub<'TAbstract when 'TAbstract : not struct> (calls:(MethodInfo * (Arg[] * Result)) list) =
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
            let attributes = TypeAttributes.Public ||| TypeAttributes.Class
            moduleBuilder.DefineType(stubName, attributes, parent, interfaces)
        /// Field settings
        let fields = FieldAttributes.Private ||| FieldAttributes.InitOnly 
        /// Field for method return values
        let returnValuesField = typeBuilder.DefineField("_returnValues", typeof<obj[]>, fields)
        /// Field for method arguments 
        let argsField = typeBuilder.DefineField("_args", typeof<obj[][]>, fields)
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
            /// Method builder
            let methodBuilder = defineMethod typeBuilder abstractMethod
            /// IL generator
            let gen = methodBuilder.GetILGenerator()
            /// Method overloads defined for current method
            let overloads = groupedMethods |> Seq.tryFind (fst >> (=) abstractMethod)
            match overloads with
            | Some (_, overloads) ->
                overloads |> Seq.toList |> List.rev |> Seq.iter (fun (_,(args, result)) ->
                    /// Label to goto if argument fails
                    let unmatched = gen.DefineLabel()
                    /// Index of argument values for current method overload
                    let argsLookupIndex = argsLookup.Count
                    // Add arguments to lookup
                    args |> Array.map (function Any -> null | Arg(value) -> value) |> argsLookup.Add
                    // Emit argument matching
                    args |> Seq.iteri (fun argIndex arg ->
                        match arg with
                        | Any -> ()
                        | Arg(value) ->
                            // Emit Object.Equals(box args.[argIndex+1], _args.[argsLookupIndex].[argIndex])
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
                        // Emit _returnValues.[returnValuesIndex]
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
                if abstractMethod.ReturnType = typeof<System.Void> 
                then gen.Emit(OpCodes.Ret)
                else gen.ThrowException(typeof<NotImplementedException>)
            if abstractType.IsInterface then
                typeBuilder.DefineMethodOverride(methodBuilder, abstractMethod)
        /// Stub type
        let stubType = typeBuilder.CreateType()
        /// Generated object instance
        let generatedObject = 
            Activator.CreateInstance(
                stubType, [|box (returnValues.ToArray());box (argsLookup.ToArray())|])
        generatedObject :?> 'TAbstract

open CodeEmit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

/// Wildcard attribute
[<AttributeUsage(AttributeTargets.Method)>]
type WildcardAttribute() = inherit Attribute()

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
            | Value(v,t) | Coerce(Value(v,t),_) -> Arg(v)
            | Call(_,mi, _) when isWildcard mi -> Any
            | _ -> raise <| NotSupportedException(arg.ToString()) |]
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toCall = function
        | Call(Some(x), mi, args) when x.Type = abstractType -> mi, toArgs args
        | PropertyGet(Some(x), pi, args) when x.Type = abstractType -> pi.GetGetMethod(), toArgs args
        | PropertySet(Some(x), pi, args, value) when x.Type = abstractType -> pi.GetSetMethod(), toArgs args
        | expr -> raise <| NotSupportedException(expr.ToString())
    /// Default constructor
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
    let mi, args = call
    /// Specifies the return value of a method
    member this.Returns(value:'TReturnValue) =
        let result = if typeof<'TReturnValue> = typeof<unit> then Unit else ReturnValue(value)
        Stub<'TAbstract>((mi, (args, result))::calls)
    /// Specifies the exception a method raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) 
                                   and  'TException :> exn>() =
        Stub<'TAbstract>((mi, (args, Raise(typeof<'TException>)))::calls)

[<Sealed>]
type It private () =
    /// Marks argument as accepting any value
    [<Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module It =
    /// Marks argument as accepting any value
    let [<Wildcard>] inline any () : 'TArg = It.IsAny()