﻿namespace Fock

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
#if MIN_DEPENDENCY
#else
open Microsoft.FSharp.Linq.QuotationEvaluation // F# PowerPack dependency
#endif

module internal CodeEmit =
    /// Boxed value
    type Value = obj
    /// Boxed function
    type Func = obj
    /// Boxed event
    type PublishedEvent = obj
    /// Method argument type
    type Arg = Any | Arg of Value | Pred of Func | PredUntyped of Func
    /// Method result type
    type Result = 
        | Unit
        | ReturnValue of Value * Type
        | ReturnFunc of Func
        | Handler of string * PublishedEvent
        | Call of Func
        | Raise of Type
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
    /// Generate method args match
    let generateArgs 
        (gen:ILGenerator) (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder) 
        (mi:MethodInfo,args) (unmatched:Label) =
        /// Index of argument values for current method overload
        let argsLookupIndex = argsLookup.Count
        // Add arguments to lookup
        args |> Array.map (function Any -> null | Arg(value) -> value | Pred(f) -> f | PredUntyped(f) -> f) |> argsLookup.Add
        // Emit argument matching
        args |> Seq.iteri (fun argIndex arg ->
            let emitArgBox () =
                gen.Emit(OpCodes.Ldarg, argIndex+1)
                gen.Emit(OpCodes.Box, mi.GetParameters().[argIndex].ParameterType)
            let emitArgLookup value =
                gen.Emit(OpCodes.Ldarg_0)
                gen.Emit(OpCodes.Ldfld, argsField)
                gen.Emit(OpCodes.Ldc_I4, argsLookupIndex)
                gen.Emit(OpCodes.Ldelem_Ref)
                gen.Emit(OpCodes.Ldc_I4, argIndex)
                gen.Emit(OpCodes.Ldelem_Ref)
            match arg with
            | Any -> ()
            | Arg(value) ->
                emitArgBox ()
                emitArgLookup value
                // Emit Object.Equals(box args.[argIndex+1], _args.[argsLookupIndex].[argIndex])
                gen.EmitCall(OpCodes.Call, typeof<obj>.GetMethod("Equals",[|typeof<obj>;typeof<obj>|]), null) 
                gen.Emit(OpCodes.Brfalse_S, unmatched)
            | Pred(f) ->
                emitArgLookup f
                gen.Emit(OpCodes.Ldarg, argIndex+1)
                let argType = mi.GetParameters().[argIndex].ParameterType
                let invoke = FSharpType.MakeFunctionType(argType,typeof<bool>).GetMethod("Invoke")
                gen.Emit(OpCodes.Callvirt, invoke)
                gen.Emit(OpCodes.Brfalse_S, unmatched)
            | PredUntyped(f) ->
                emitArgLookup f
                gen.Emit(OpCodes.Ldarg, argIndex+1)
                let argType = mi.GetParameters().[argIndex].ParameterType
                gen.Emit(OpCodes.Box, argType)
                let invoke = FSharpType.MakeFunctionType(typeof<obj>,typeof<bool>).GetMethod("Invoke")
                gen.Emit(OpCodes.Callvirt, invoke)
                gen.Emit(OpCodes.Brfalse_S, unmatched)
        )
    /// Generates method return
    let generateReturn 
        (gen:ILGenerator) (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder)
        (mi:MethodInfo,result) =
        /// Emits _returnValues.[returnValuesIndex]
        let emitReturnValueLookup value =
            let returnValuesIndex = returnValues.Count
            returnValues.Add(value)
            gen.Emit(OpCodes.Ldarg_0)
            gen.Emit(OpCodes.Ldfld, returnValuesField)
            gen.Emit(OpCodes.Ldc_I4, returnValuesIndex)
            gen.Emit(OpCodes.Ldelem_Ref)
        /// Emits AddHandler/RemoveHandler
        let emitEventHandler handlerName e =
            emitReturnValueLookup e
            let handlerType = e.GetType().GetGenericArguments().[0]
            gen.Emit(OpCodes.Ldarg_1)
            let t = typedefof<IDelegateEvent<_>>.MakeGenericType(handlerType)
            let invoke = t.GetMethod(handlerName)
            gen.Emit(OpCodes.Callvirt, invoke)
            gen.Emit(OpCodes.Ret)
        // Emit result
        match result with
        | Unit -> gen.Emit(OpCodes.Ret)
        | ReturnValue(value, returnType) ->
            emitReturnValueLookup value
            gen.Emit(OpCodes.Unbox_Any, returnType)
            gen.Emit(OpCodes.Ret)
        | ReturnFunc(f) ->
            emitReturnValueLookup f
            // Emit Invoke
            gen.Emit(OpCodes.Ldnull)
            let invoke = typeof<FSharpFunc<unit,obj>>.GetMethod("Invoke")
            gen.Emit(OpCodes.Callvirt, invoke)
            if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then 
                gen.Emit(OpCodes.Pop)
            gen.Emit(OpCodes.Ret)
        | Handler(handlerName, e) -> emitEventHandler handlerName e
        | Call(f) ->
            emitReturnValueLookup f
            // Emit Invoke
            let args = mi.GetParameters() |> Array.map (fun arg -> arg.ParameterType)
            if args.Length = 1 then gen.Emit(OpCodes.Ldarg_1)
            else
                for i = 1 to args.Length do gen.Emit(OpCodes.Ldarg, i)
                gen.Emit(OpCodes.Newobj, FSharpType.MakeTupleType(args).GetConstructor(args))
            let invoke = typeof<FSharpFunc<obj,obj>>.GetMethod("Invoke")
            gen.Emit(OpCodes.Callvirt, invoke)
            if mi.ReturnType = typeof<unit> || mi.ReturnType = typeof<Void> then gen.Emit(OpCodes.Pop)
            gen.Emit(OpCodes.Ret)
        | Raise(excType) -> gen.ThrowException(excType)
    /// Generates method
    let generateMethod 
        (gen:ILGenerator)
        (argsLookup:ResizeArray<Value[]>,argsField:FieldBuilder)
        (returnValues:ResizeArray<Value>,returnValuesField:FieldBuilder) 
        (mi:MethodInfo,(args, result)) =
        /// Label to goto if argument fails
        let unmatched = gen.DefineLabel()
        generateArgs gen (argsLookup,argsField) (mi,args) unmatched
        generateReturn gen (returnValues,returnValuesField) (mi,result)
        gen.MarkLabel(unmatched)
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
        let argsLookup = ResizeArray<Value[]>()
        /// Method return values
        let returnValues = ResizeArray<Value>()
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
            let toMethod = generateMethod (gen) (argsLookup,argsField) (returnValues,returnValuesField)
            match overloads with
            | Some (_, overloads) ->
                overloads |> Seq.toList |> List.rev |> Seq.iter toMethod
                gen.ThrowException(typeof<MatchFailureException>)
            | None ->
                if abstractMethod.ReturnType = typeof<System.Void> || abstractMethod.ReturnType = typeof<unit>
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

/// Predicate attribute
[<AttributeUsage(AttributeTargets.Method)>]
type PredicateAttribute() = inherit Attribute()

/// Generic stub type over abstract type
type Stub<'TAbstract when 'TAbstract : not struct> internal (calls) =
    /// Abstract type
    let abstractType = typeof<'TAbstract>
    /// Converts argument expressions to Arg array
    let toArgs args =
        let hasAttribute a (mi:MethodInfo) = mi.GetCustomAttributes(a, true).Length > 0
        [|for arg in args ->
            match arg with
            | Call(_, mi, _) when hasAttribute typeof<WildcardAttribute> mi -> Any
#if MIN_DEPENDENCY
            | Value(v,t) | Coerce(Value(v,t),_) -> Arg(v)
            | FieldGet(Some(Value(v,_)),fi) -> fi.GetValue(v) |> Arg
            | PropertyGet(None, pi, []) -> pi.GetValue(null,[||]) |> Arg
            | PropertyGet(Some(Value(v,_)),pi,[]) -> pi.GetValue(v,[||]) |> Arg
            | _ -> raise <| NotSupportedException(arg.ToString()) |]
#else
            | Call(_, mi, [pred]) when hasAttribute typeof<PredicateAttribute> mi -> Pred(pred.EvalUntyped())
            | expr -> expr.EvalUntyped() |> Arg |]
#endif
    /// Converts expression to a tuple of MethodInfo and Arg array
    let toCall = function
        | Call(Some(x), mi, args) when x.Type = abstractType -> mi, toArgs args
        | PropertyGet(Some(x), pi, args) when x.Type = abstractType -> pi.GetGetMethod(), toArgs args
        | PropertySet(Some(x), pi, args, value) when x.Type = abstractType -> pi.GetSetMethod(), toArgs args
        | expr -> raise <| NotSupportedException(expr.ToString())
    let toHandlers = function
        | Call(None, mi, [Lambda(_,Call(Some(x),addHandler,_));
                          Lambda(_,Call(Some(_),removeHandler,_));_]) when x.Type = abstractType -> 
            addHandler, removeHandler
        | expr -> raise <| NotSupportedException(expr.ToString())
    /// Default constructor
    new () = Stub([])
    /// Specifies a method of the abstract type as a quotation
    member this.Method(f:'TAbstract -> Expr<'TReturnValue>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let call = toCall (f default')
        ResultBuilder<'TAbstract,'TReturnValue>(call,calls)
    /// Specifies an event of the abstract type as a quotation
    member this.Event(f:'TAbstract -> Expr<'TEvent>) =
        let default' = Unchecked.defaultof<'TAbstract>
        let handlers = toHandlers (f default')
        EventBuilder<'TAbstract,'TEvent>(handlers,calls)
    /// Creates an instance of the abstract type
    member this.Create() = stub<'TAbstract>(calls)
/// Generic builder for specifying method results
and ResultBuilder<'TAbstract,'TReturnValue when 'TAbstract : not struct> 
    internal (call, calls) =
    let mi, args = call
    /// Specifies the return value of a method
    member this.Returns(value:'TReturnValue) =
        let result = if typeof<'TReturnValue> = typeof<unit> then Unit else ReturnValue(value,typeof<'TReturnValue>)
        Stub<'TAbstract>((mi, (args, result))::calls)
    /// Specifies a computed return value of a method
    member this.Returns(f:unit -> 'TReturnVaue) =
        Stub<'TAbstract>((mi, (args, ReturnFunc(f)))::calls)
    /// Calls the specified function to obtain the return value
    [<RequiresExplicitTypeArguments>]
    member this.Calls<'TArgs>(f:'TArgs -> 'TReturnValue) =
        Stub<'TAbstract>((mi, (args, Call(f)))::calls)
    /// Specifies the exception a method raises
    [<RequiresExplicitTypeArguments>]
    member this.Raises<'TException when 'TException : (new : unit -> 'TException) 
                                   and  'TException :> exn>() =
        Stub<'TAbstract>((mi, (args, Raise(typeof<'TException>)))::calls)
/// Generic builder for specifying event values
and EventBuilder<'TAbstract,'TEvent when 'TAbstract : not struct> 
    internal (handlers, calls) =
    let add, remove = handlers
    /// Specifies the published event value
    member this.Publishes(value:'TEvent) =
        Stub<'TAbstract>((add, ([|Any|], Handler("AddHandler",value)))::
                         (remove, ([|Any|], Handler("RemoveHandler",value)))::
                         calls)

[<Sealed>]
type It private () =
    /// Marks argument as matching any value
    [<Wildcard>] static member IsAny<'TArg>() = Unchecked.defaultof<'TArg>
    /// Marks argument as matching specific values
    [<Predicate>] static member Is<'TArg>(f:'TArg -> bool) = Unchecked.defaultof<'TArg>

[<AutoOpen;CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module It =
    /// Marks argument as accepting any value
    let [<Wildcard>] inline any () : 'TArg = It.IsAny()
#if MIN_DEPENDENCY
#else
    let [<Predicate>] inline is (f) : 'TArg = It.Is(f)
#endif