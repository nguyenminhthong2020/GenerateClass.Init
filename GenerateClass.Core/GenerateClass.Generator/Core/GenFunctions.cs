using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.DependencyInjection;
using System.Collections.Immutable;
using System.Reflection;
using System.Text;

namespace GenerateClass.Generator.Core
{
    public static class GenFunctions
    {
        /// <summary>
        /// Parse a single new GenerateClassOption() { ... } into GenerateClassOption
        /// </summary>
        /// <param name="obj"></param>
        /// <param name="model"></param>
        /// <returns></returns>
        public static GenerateClassOption ParseGenerateClassOptionObject(ObjectCreationExpressionSyntax obj, SemanticModel model)
        {
            var opt = new GenerateClassOption();

            if (obj.Initializer is null)
                return opt;

            foreach (var init in obj.Initializer.Expressions)
            {
                if (init is AssignmentExpressionSyntax assign)
                {
                    string name = (assign.Left as IdentifierNameSyntax)?.Identifier.Text ?? "";
                    if (string.IsNullOrEmpty(name) && assign.Left is MemberAccessExpressionSyntax ma)
                        name = ma.Name.Identifier.Text;

                    if (name == "ConfigId" && assign.Right is LiteralExpressionSyntax litId)
                        opt.ConfigId = litId.Token.ValueText;
                    else if (name == "Path" && assign.Right is LiteralExpressionSyntax lit)
                        opt.Path = lit.Token.ValueText;
                    else if (name == "Enabled")
                        opt.Enabled = assign.Right.IsKind(SyntaxKind.TrueLiteralExpression);
                    else if (name == "Namespace" && assign.Right is LiteralExpressionSyntax ns)
                        opt.Namespace = ns.Token.ValueText;
                    else if (name == "RequiredProperties")
                    {
                        opt.RequiredProperties ??= new Dictionary<string, object>();
                        ParseRequiredProperties(assign.Right, model, opt.RequiredProperties);
                    }
                }
            }
            return opt;
        }

        /// <summary>
        /// Parse the RequiredProperties initializer (both { "Id","long" } and ["Id"]=typeof(long))
        /// </summary>
        /// <param name="right"></param>
        /// <param name="model"></param>
        /// <param name="dict"></param>
        public static void ParseRequiredProperties(ExpressionSyntax right, SemanticModel model, Dictionary<string, object> dict)
        {
            InitializerExpressionSyntax? initializer = null;

            if (right is ObjectCreationExpressionSyntax oce)
                initializer = oce.Initializer;
            else if (right is ImplicitObjectCreationExpressionSyntax ioce)
                initializer = ioce.Initializer;
            else if (right is InitializerExpressionSyntax directInit)
                initializer = directInit;

            if (initializer is null) return;

            foreach (ExpressionSyntax element in initializer.Expressions)
            {
                // Case A: { "Id", "long" } / { "Id", typeof(string) }
                if (element is InitializerExpressionSyntax pair && pair.Expressions.Count == 2)
                {
                    var keyExpr = pair.Expressions[0];
                    var valExpr = pair.Expressions[1];

                    string? key = (keyExpr as LiteralExpressionSyntax)?.Token.ValueText
                                  ?? TryGetStringFromExpression(keyExpr, model);

                    object? valObj = null;

                    if (valExpr is LiteralExpressionSyntax les)
                        valObj = les.Token.ValueText;
                    else if (valExpr is TypeOfExpressionSyntax tos)
                    {
                        var tSym = model.GetTypeInfo(tos.Type).Type;
                        if (tSym is not null) valObj = GetFriendlyTypeName(tSym);
                    }
                    else
                        valObj = TryGetStringFromExpression(valExpr, model) ?? valExpr.ToString().Trim('"');

                    if (!string.IsNullOrEmpty(key) && valObj is not null)
                        dict[key!] = valObj;
                }
                // Case B: index initializer ["Id"] = typeof(long)
                else if (element is AssignmentExpressionSyntax idxAssign)
                {
                    string? key = null;
                    ExpressionSyntax leftExpr = idxAssign.Left;
                    ExpressionSyntax? keyExpr = null;

                    if (leftExpr is ElementAccessExpressionSyntax eae)
                        keyExpr = eae.ArgumentList.Arguments.FirstOrDefault()?.Expression;
                    else
                    {
                        var bracket = leftExpr.ChildNodes().OfType<BracketedArgumentListSyntax>().FirstOrDefault();
                        if (bracket is not null)
                            keyExpr = bracket.Arguments.FirstOrDefault()?.Expression;
                    }

                    if (keyExpr is not null)
                    {
                        key = (keyExpr as LiteralExpressionSyntax)?.Token.ValueText
                              ?? TryGetStringFromExpression(keyExpr, model)
                              ?? keyExpr.ToString().Trim('"');
                    }

                    object? valObj = null;
                    var valueSide = idxAssign.Right;

                    if (valueSide is LiteralExpressionSyntax les2)
                        valObj = les2.Token.ValueText;
                    else if (valueSide is TypeOfExpressionSyntax tos2)
                    {
                        var tSym2 = model.GetTypeInfo(tos2.Type).Type;
                        if (tSym2 is not null) valObj = GetFriendlyTypeName(tSym2);
                    }
                    else
                        valObj = TryGetStringFromExpression(valueSide, model) ?? valueSide.ToString().Trim('"');

                    if (!string.IsNullOrEmpty(key) && valObj is not null)
                        dict[key!] = valObj;
                }
            }
        }

        /// <summary>
        /// Attempts to extract a string representation from a given expression syntax node.
        /// Handles various expression types such as literals, typeof, nameof, constants, and identifiers.
        /// </summary>
        /// <param name="expr"></param>
        /// <param name="sm"></param>
        /// <returns></returns>
        public static string? TryGetStringFromExpression(ExpressionSyntax? expr, SemanticModel sm)
        {
            if (expr is null)
                return null;

            // string literal "abc"
            if (expr is LiteralExpressionSyntax lit)
                return lit.Token.ValueText;

            // typeof(int) => "int"
            if (expr is TypeOfExpressionSyntax tos)
            {
                var tSym = sm.GetTypeInfo(tos.Type).Type;
                if (tSym is not null)
                    return GetFriendlyTypeName(tSym);
            }

            // nameof(Xyz) => "Xyz"
            if (expr is InvocationExpressionSyntax inv &&
                inv.Expression is IdentifierNameSyntax idn &&
                idn.Identifier.Text == "nameof" &&
                inv.ArgumentList.Arguments.Count == 1)
            {
                return inv.ArgumentList.Arguments[0].ToString();
            }

            // constants/fields/locals with constant values
            var sym = sm.GetSymbolInfo(expr).Symbol;
            if (sym is IFieldSymbol f && f.HasConstantValue)
                return f.ConstantValue?.ToString();
            if (sym is ILocalSymbol l && l.HasConstantValue)
                return l.ConstantValue?.ToString();

            // fallback: try semantic constant value
            var constVal = sm.GetConstantValue(expr);
            if (constVal.HasValue)
                return constVal.Value?.ToString();

            // identifier as text
            if (expr is IdentifierNameSyntax id)
                return id.Identifier.Text;

            // last resort: ToString()
            return expr.ToString().Trim('\"');
        }

        /// <summary>
        /// Returns a simplified, human-readable name for a given type symbol.
        /// Handles common aliases, nullable types, arrays, tuples, generics, and nullable reference types.
        /// </summary>
        /// <param name="t"></param>
        /// <returns></returns>
        public static string GetFriendlyTypeName(ITypeSymbol t)
        {
            // use aliases for common types
            switch (t.SpecialType)
            {
                case SpecialType.System_Boolean: return "bool";
                case SpecialType.System_Byte: return "byte";
                case SpecialType.System_Char: return "char";
                case SpecialType.System_Decimal: return "decimal";
                case SpecialType.System_Double: return "double";
                case SpecialType.System_Single: return "float";
                case SpecialType.System_Int16: return "short";
                case SpecialType.System_Int32: return "int";
                case SpecialType.System_Int64: return "long";
                case SpecialType.System_String: return "string";
                case SpecialType.System_Object: return "object";
                case SpecialType.System_SByte: return "sbyte";
                case SpecialType.System_UInt16: return "ushort";
                case SpecialType.System_UInt32: return "uint";
                case SpecialType.System_UInt64: return "ulong";
                case SpecialType.System_IntPtr: return "nint";
                case SpecialType.System_UIntPtr: return "nuint";
            }

            // nullable
            if (t is INamedTypeSymbol namedNullable &&
                namedNullable.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T)
            {
                var inner = namedNullable.TypeArguments[0];
                return GetFriendlyTypeName(inner) + "?";
            }

            // array
            // multi-dimensional array, jagged array
            if (t is IArrayTypeSymbol arr)
            {
                var elem = GetFriendlyTypeName(arr.ElementType);
                if (arr.Rank == 1)
                    return elem + "[]";
                else
                    return elem + "[" + new string(',', arr.Rank - 1) + "]";
            }

            // tuple type
            // ValueTuple<int, string, string, bool>  ==> (int, string, string, bool) 
            if (t is INamedTypeSymbol tupleType && tupleType.IsTupleType)
            {
                // NEW: normalize each tuple element: if anonymous or null -> object
                // (1, null, "abc") --> (int, object, string)
                var elements = tupleType.TupleElements
                    .Select(e =>
                    {
                        var et = e.Type;
                        if (et is null || et.IsAnonymousType || (et is INamedTypeSymbol en && en.IsAnonymousType))
                            return "object";
                        return GetFriendlyTypeName(et);
                    });
                return $"({string.Join(", ", elements)})";
            }

            // generic type: List<Student>
            if (t is INamedTypeSymbol named && named.IsGenericType)
            {
                var name = named.Name; // List, Dictionary, etc
                var args = string.Join(", ", named.TypeArguments.Select(GetFriendlyTypeName));
                return $"{name}<{args}>";
            }

            // nullable reference types: string?, List<Student>?
            // (C# 8+ with #nullable enable)
            if (t.NullableAnnotation == NullableAnnotation.Annotated && !t.IsValueType)
            {
                var bare = t.WithNullableAnnotation(NullableAnnotation.None);
                return GetFriendlyTypeName(bare) + "?";
            }

            // fallback to minimally qualified (e.g. MyNamespace.MyType)
            return t.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).Replace("global::", "");
        }

        /// <summary>
        /// Returns a simplified, human-readable name for a given System.Type.
        /// Handles common aliases, nullable types, arrays, tuples, generics.
        /// </summary>
        public static string GetSimpleTypeName(Type t)
        {
            if (t == null)
                return "object";

            // nullable types: Nullable<T>
            if (Nullable.GetUnderlyingType(t) is Type innerNullable)
            {
                return GetSimpleTypeName(innerNullable) + "?";
            }

            // array (multi-dim & jagged)
            if (t.IsArray)
            {
                var elem = GetSimpleTypeName(t.GetElementType()!);
                if (t.GetArrayRank() == 1)
                    return elem + "[]";
                else
                    return elem + "[" + new string(',', t.GetArrayRank() - 1) + "]";
            }

            // aliases for common types
            if (t == typeof(bool)) return "bool";
            if (t == typeof(byte)) return "byte";
            if (t == typeof(char)) return "char";
            if (t == typeof(decimal)) return "decimal";
            if (t == typeof(double)) return "double";
            if (t == typeof(float)) return "float";
            if (t == typeof(short)) return "short";
            if (t == typeof(int)) return "int";
            if (t == typeof(long)) return "long";
            if (t == typeof(string)) return "string";
            if (t == typeof(object)) return "object";
            if (t == typeof(sbyte)) return "sbyte";
            if (t == typeof(ushort)) return "ushort";
            if (t == typeof(uint)) return "uint";
            if (t == typeof(ulong)) return "ulong";
            if (t == typeof(nint)) return "nint";
            if (t == typeof(nuint)) return "nuint";
            if (t == typeof(void)) return "void";

            // ValueTuple<T1,T2,...> → (T1,T2,...)
            if (t.IsGenericType && t.FullName!.StartsWith("System.ValueTuple"))
            {
                var elements = t.GetGenericArguments()
                                .Select(a => GetSimpleTypeName(a));
                return $"({string.Join(", ", elements)})";
            }

            // generic type: List<Student>, Dictionary<int,string>
            if (t.IsGenericType)
            {
                var name = t.Name;
                var backtick = name.IndexOf('`');
                if (backtick >= 0) name = name.Substring(0, backtick);
                var args = string.Join(", ", t.GetGenericArguments().Select(GetSimpleTypeName));
                return $"{name}<{args}>";
            }

            // fallback to namespace.name
            return string.IsNullOrEmpty(t.Namespace) ? t.Name : $"{t.Namespace}.{t.Name}";
        }

        // /// <summary>
        // /// Safely infer a non-null ITypeSymbol for a given ExpressionSyntax.
        // /// - Handles TupleExpressionSyntax (including nested tuples) by constructing a tuple symbol
        // ///   with normalized element types (fallback to object for null/anonymous).
        // /// - Returns object for null literal or when the type cannot be resolved.
        // /// </summary>
        public static ITypeSymbol GetExpressionTypeSafe(ExpressionSyntax expr, SemanticModel sm)
        {
            if (expr is null)
                return sm.Compilation.GetSpecialType(SpecialType.System_Object);

            // 1) Tuple literal, e.g. (1, "a", (2, null))
            if (expr is TupleExpressionSyntax tupleExpr)
            {
                // Collect element types and names (if provided)
                var elemTypesBuilder = ImmutableArray.CreateBuilder<ITypeSymbol>(tupleExpr.Arguments.Count);
                var namesBuilder = ImmutableArray.CreateBuilder<string?>(tupleExpr.Arguments.Count);
                bool anyNames = false;

                foreach (var arg in tupleExpr.Arguments)
                {
                    // Recursively get element type (handles nested tuple)
                    var elemType = GetExpressionTypeSafe(arg.Expression, sm) ?? sm.Compilation.GetSpecialType(SpecialType.System_Object);

                    // Normalize anonymous to object if needed (optional: you may call your NormalizeTypeAnonymousSafe here)
                    // elemType = NormalizeTypeAnonymousSafe(elemType, sm, arg.Expression);

                    elemTypesBuilder.Add(elemType);

                    string? name = arg.NameColon?.Name.Identifier.ValueText;
                    if (!string.IsNullOrEmpty(name))
                    {
                        anyNames = true;
                        namesBuilder.Add(name);
                    }
                    else
                    {
                        // add null for unnamed elements — IMPORTANT: use null, not empty string
                        namesBuilder.Add(null);
                    }
                }

                var elemTypes = elemTypesBuilder.MoveToImmutable();

                try
                {
                    if (!anyNames)
                    {
                        // no element names — use overload without names
                        return sm.Compilation.CreateTupleTypeSymbol(elemTypes);
                    }
                    else
                    {
                        // create ImmutableArray<string> that may contain nulls for unnamed elements
                        var namesArrBuilder = ImmutableArray.CreateBuilder<string>(namesBuilder.Count);
                        foreach (var nm in namesBuilder)
                        {
                            // namesBuilder contains string? — it's ok to add null here
#pragma warning disable CS8604 // Possible null reference argument.
                            namesArrBuilder.Add(nm);
#pragma warning restore CS8604 // Possible null reference argument.
                        }

                        var namesArr = namesArrBuilder.MoveToImmutable();
#pragma warning disable CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
                        return sm.Compilation.CreateTupleTypeSymbol(elemTypes, namesArr);
#pragma warning restore CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
                    }
                }
                catch
                {
                    // If for some reason creating a tuple symbol fails, fallback to object
                    return sm.Compilation.GetSpecialType(SpecialType.System_Object);
                }
            }

            // 2) null literal => object
            var constVal = sm.GetConstantValue(expr);
            if (constVal.HasValue && constVal.Value == null)
                return sm.Compilation.GetSpecialType(SpecialType.System_Object);

            // 3) Default: use semantic model
            var t = sm.GetTypeInfo(expr).Type;
            if (t != null)
                return t;

            // fallback
            return sm.Compilation.GetSpecialType(SpecialType.System_Object);
        }

        /// <summary>
        /// Decide if a named type should be generated by the generator.
        /// Return false for primitives, void, compiler/core/CLR types and anonymous types.
        /// </summary>
        public static bool ShouldGenerateType(INamedTypeSymbol? t)
        {
            if (t is null)
                return false;

            // 1) Skip anonymous compiler-generated types
            if (t.IsAnonymousType)
                return false;

            // 2) Skip built-in special types (int, string, void, bool, ...)
            if (t.SpecialType != SpecialType.None)
                return false;

            // 3) Skip common runtime/core/library assemblies (avoid generating types from BCL)
            var asm = t.ContainingAssembly?.Name ?? string.Empty;
            if (asm.Equals("System.Private.CoreLib", StringComparison.OrdinalIgnoreCase)
                || asm.Equals("mscorlib", StringComparison.OrdinalIgnoreCase)
                || asm.Equals("netstandard", StringComparison.OrdinalIgnoreCase))
            {
                return false;
            }

            // 4) Skip types in System.* (and other configured prefixes)
            var ns = t.ContainingNamespace?.ToDisplayString() ?? "";
            if (!string.IsNullOrEmpty(ns))
            {
                // If you have an IgnoreType._skipPrefixes list, use it:
                if (IgnoreType._skipPrefixes.Any(pre => ns.StartsWith(pre, StringComparison.Ordinal)))
                    return false;

                // Minimal conservative check:
                if (ns.StartsWith("System", StringComparison.Ordinal))
                    return false;
            }

            return true; 
        }

        /// <summary>
        /// Find object initializers and assignments in the method body to get the properties assigned to this type.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="methodSyntax"></param>
        /// <param name="compilation"></param>
        /// <param name="map"></param>
        /// <param name="mapCtor"></param>
        /// <param name="mapNamespaces"></param>
        /// <param name="methodSymbol"></param>
        public static void CollectTypeAndMembers(
        ITypeSymbol type,
        MethodDeclarationSyntax methodSyntax,
        Compilation compilation,
        Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> map,
        Dictionary<INamedTypeSymbol, List<(string CtorKey,
                                   List<(string? Name, ITypeSymbol Type)> Params,
                                   bool IsNamedArgs)>> mapCtor,
        Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces,
        IMethodSymbol methodSymbol)
        {
            if (type is null)
                return;

            // 1) Unwrap arrays/collections/tuples/generics and recursively collect inner types first
            // This ensures we find ResponseClass1, Test2, MyNewClass, etc.
            // Handle arrays
            if (type is IArrayTypeSymbol arr)
            {
                CollectTypeAndMembers(arr.ElementType, methodSyntax, compilation, map, mapCtor, mapNamespaces, methodSymbol);
                return;
            }

            // If this is a tuple type, collect all element types
            if (type is INamedTypeSymbol namedTuple && namedTuple.IsTupleType)
            {
                foreach (var el in namedTuple.TupleElements)
                    CollectTypeAndMembers(el.Type, methodSyntax, compilation, map, mapCtor, mapNamespaces, methodSymbol);
                return;
            }

            // If named generic, first recurse into type arguments (List<T>, Dictionary<K,V>, TestGenericClass<T> ...)
            if (type is INamedTypeSymbol namedGeneric && namedGeneric.IsGenericType)
            {
                foreach (var targ in namedGeneric.TypeArguments)
                    CollectTypeAndMembers(targ, methodSyntax, compilation, map, mapCtor, mapNamespaces, methodSymbol);
                // Continue below to also consider the generic type itself (e.g. TestGenericClass<...>)
            }

            // Now try to get named type for the current symbol (unwrapping possible collection wrapper)
            INamedTypeSymbol? namedType = UnwrapCollection(type) ?? type as INamedTypeSymbol;

            // 2. Filter
            if (!ShouldGenerateType(namedType))
                return;

            // 3) Ensure entry in map for this namedType (we keep the INamedTypeSymbol as key so we
            //    can preserve generic information for generation)
            if (!map.ContainsKey(namedType!))
                map[namedType!] = new Dictionary<string, ITypeSymbol?>();

            // Find object initializers in the method body that match this type:
            SemanticModel semanticModel = compilation.GetSemanticModel(methodSyntax.SyntaxTree);

            // 4) Inspect object creation initializers: new X { Prop = ... }
            // new Teacher(){...}
            IEnumerable<ObjectCreationExpressionSyntax> objectCreations = methodSyntax.DescendantNodes()
                                                                      .OfType<ObjectCreationExpressionSyntax>();
            foreach (ObjectCreationExpressionSyntax creation in objectCreations)
            {
                INamedTypeSymbol? createdType = semanticModel.GetTypeInfo(creation).Type as INamedTypeSymbol;
                INamedTypeSymbol? unwrapped = UnwrapCollection(createdType) ?? createdType;

                if (SymbolEqualityComparer.Default.Equals(unwrapped, namedType))
                {
                    // Get the assignments in the initializer:
                    // initializer form: new T { Prop = value, ... }
                    if (creation.Initializer is not null)
                    {
                        foreach (ExpressionSyntax expr in creation.Initializer.Expressions)
                        {
                            if (expr is AssignmentExpressionSyntax assign)
                            {
                                // use semantic model to get property name correctly
                                var leftSym = semanticModel.GetSymbolInfo(assign.Left).Symbol as IPropertySymbol;
                                string propName;
                                if (leftSym is not null)
                                {
                                    propName = leftSym.Name;
                                }
                                else if (assign.Left is MemberAccessExpressionSyntax ma)
                                {
                                    // e.g. obj.Prop  or this.Prop
                                    propName = ma.Name.Identifier.Text;
                                }
                                else if (assign.Left is IdentifierNameSyntax idn)
                                {
                                    // e.g. Prop (maybe inside same class)
                                    propName = idn.Identifier.Text;
                                }
                                else
                                {
                                    // last-resort fallback
                                    propName = assign.Left.ToString().Split('.').Last();
                                }

                                /*
                                 * Resolve right-hand expression type safely (handles tuples, anonymous, null)
                                */
                                //ITypeSymbol? rightType = semanticModel.GetTypeInfo(assign.Right).Type;
                                var rightType = GetExpressionTypeSafe(assign.Right, semanticModel);
                                rightType = NormalizeTypeAnonymousSafe(rightType, semanticModel, assign.Right);
                                rightType = NormalizeMultiDimensionalArrayType(rightType, semanticModel);
                                map[namedType!][propName] = rightType;
                                GetNamespace(rightType, mapNamespaces, namedType!);
                            }
                        }
                    }

                    // constructor named-arg style: new T( propName: value, ...)
                    // var obj = new MyObject(Prop1: val1, Prop2: val2,...)
                    if (creation.ArgumentList is not null)
                    {
                        var ctorParamsNamed = new List<(string? Name, ITypeSymbol Type)>();
                        var ctorParamsPositional = new List<(string? Name, ITypeSymbol Type)>();
                        bool hasNamedArgs = false;

                        foreach (var arg in creation.ArgumentList.Arguments)
                        {
                            var argType = GetExpressionTypeSafe(arg.Expression, semanticModel);
                            argType = NormalizeTypeAnonymousSafe(argType, semanticModel, arg.Expression);
                            argType = NormalizeMultiDimensionalArrayType(argType, semanticModel);

                            if (arg.NameColon is not null)
                            {
                                hasNamedArgs = true;
                                string propName = arg.NameColon.Name.Identifier.Text;
                                map[namedType!][propName] = argType;
                                GetNamespace(argType, mapNamespaces, namedType!);
                                ctorParamsNamed.Add((propName, argType!));
                            }
                            else
                            {
                                ctorParamsPositional.Add((null, argType!));
                                GetNamespace(argType, mapNamespaces, namedType!);
                            }
                        }

                        if (hasNamedArgs)
                        {
                            var ctorKey = string.Join(",", ctorParamsNamed
                                                    .OrderBy(x => x.Name)
                                                    .Select(x => x.Name));

                            AddCtor(mapCtor, namedType!, ctorKey, ctorParamsNamed, isNamedArgs: true);
                        }
                        else if (ctorParamsPositional.Count > 0)
                        {
                            bool isNamedArgs = creation.ArgumentList.Arguments.Any(a => a.NameColon is not null);

                            string ctorKey = isNamedArgs
                                ? string.Join(",", creation.ArgumentList.Arguments
                                                          .Where(a => a.NameColon != null)
                                                          .Select(a => a.NameColon!.Name.Identifier.Text)
                                                          .OrderBy(x => x))
                                : string.Join(",", ctorParamsPositional.Select(p => p.Type.Name)); // positional key by type sequence

                            var paramList = new List<(string? Name, ITypeSymbol Type)>();
                            int idx = 0;
                            foreach (var arg in creation.ArgumentList.Arguments)
                            {
                                var argType = ctorParamsPositional[idx];
                                string? name = arg.NameColon?.Name.Identifier.Text;
                                paramList.Add((name, argType.Type));
                                idx++;
                            }

                            if (!mapCtor.TryGetValue(namedType!, out var list))
                            {
                                mapCtor[namedType!] = new List<(string, List<(string? Name, ITypeSymbol Type)>, bool)>
                                {
                                    (ctorKey, paramList, isNamedArgs)
                                };
                            }
                            else
                            {
                                var typeSeq = paramList.Select(p => p.Type);
                                var existing = list.FirstOrDefault(c =>
                                    c.Params.Count == paramList.Count &&
                                    c.Params.Select(p => p.Type).SequenceEqual(typeSeq, SymbolEqualityComparer.Default));

                                if (existing.Params != null)
                                {
                                    if (!existing.IsNamedArgs && isNamedArgs)
                                    {
                                        list.Remove(existing);
                                        list.Add((ctorKey, paramList, isNamedArgs));
                                    }
                                }
                                else
                                {
                                    list.Add((ctorKey, paramList, isNamedArgs));
                                }
                            }
                        }
                    }
                }
            }

            // 5) Also inspect assignment expressions in method body: obj.Prop = ...
            IEnumerable<AssignmentExpressionSyntax> assignments = methodSyntax.DescendantNodes()
                                                                    .OfType<AssignmentExpressionSyntax>();
            foreach (AssignmentExpressionSyntax assign in assignments)
            {
                ISymbol? leftSymbol = semanticModel.GetSymbolInfo(assign.Left).Symbol;
                string propName;

                // prioritize semantic symbol name when possible, otherwise parse syntax
                if (leftSymbol is not null)
                {
                    // only add if containingType matches or not resolved then still get name
                    if (leftSymbol.ContainingType is not null &&
                        SymbolEqualityComparer.Default.Equals(UnwrapCollection(leftSymbol.ContainingType), namedType))
                    {
                        propName = leftSymbol.Name;
                    }
                    else
                    {
                        continue;
                    }
                }
                else if (assign.Left is MemberAccessExpressionSyntax ma)
                {
                    // obj1.Adress
                    // check if the expression before is the name of the variable being assigned
                    // Ex: obj1 is of type Family
                    //var exprType = semanticModel.GetTypeInfo(ma.Expression).Type;
                    var exprType = GetExpressionTypeSafe(ma.Expression, semanticModel);
                    if (!SymbolEqualityComparer.Default.Equals(UnwrapCollection(exprType), namedType))
                        continue;

                    propName = ma.Name.Identifier.Text;
                }
                else
                {
                    // fallback — but must check type first
                    // var exprType = semanticModel.GetTypeInfo(assign.Left).Type;
                    var exprType = GetExpressionTypeSafe(assign.Left, semanticModel);
                    if (!SymbolEqualityComparer.Default.Equals(UnwrapCollection(exprType), namedType))
                        continue;

                    propName = assign.Left.ToString().Split('.').Last();
                }

                // ITypeSymbol? rightType = semanticModel.GetTypeInfo(assign.Right).Type;
                var rightType = GetExpressionTypeSafe(assign.Right, semanticModel);
                rightType = NormalizeTypeAnonymousSafe(rightType, semanticModel, assign.Right);
                rightType = NormalizeMultiDimensionalArrayType(rightType, semanticModel);
                map[namedType!][propName] = rightType;
                GetNamespace(rightType, mapNamespaces, namedType!);
            }
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="mapCtor"></param>
        /// <param name="namedType"></param>
        /// <param name="ctorKey"></param>
        /// <param name="params"></param>
        /// <param name="isNamedArgs"></param>
        public static void AddCtor(
        Dictionary<INamedTypeSymbol, List<(string CtorKey,
                                          List<(string? Name, ITypeSymbol Type)> Params,
                                          bool IsNamedArgs)>> mapCtor,
        INamedTypeSymbol namedType,
        string ctorKey,
        List<(string? ParamName, ITypeSymbol PropType)> @params,
        bool isNamedArgs)
        {
            if (!mapCtor.TryGetValue(namedType, out var list))
            {
                list = new List<(string, List<(string?, ITypeSymbol)>, bool)>();
                mapCtor[namedType] = list;
            }

            // If there are two constructors with the same key but one is namedArgs
            // and the other is positional → give priority to namedArgs
            if (list.Any(c => c.CtorKey == ctorKey && c.IsNamedArgs))
                return;

            // If there is no or only positional → add new
            if (!list.Any(c => c.CtorKey == ctorKey))
                list.Add((ctorKey, @params, isNamedArgs));
        }

        /// <summary>
        /// Normalizes a tuple type recursively, handling unresolved tuple literals,
        /// resolved tuple types, and delegating to anonymous type normalization when necessary.
        /// </summary>
        /// <param name="type">The type symbol to normalize, which may represent a tuple or an error type.</param>
        /// <param name="semanticModel">The semantic model used for type resolution.</param>
        /// <param name="expr">The expression syntax associated with the type, used for deeper analysis.</param>
        /// <returns>
        /// A normalized <see cref="ITypeSymbol"/>
        /// </returns>
        private static ITypeSymbol NormalizeTupleRecursive(
            ITypeSymbol? type,
            SemanticModel semanticModel,
            ExpressionSyntax? expr)
        {
            // case: unresolved tuple literal
            // *** handle tuple expression with error type ***
            // (1, null, "abc")
            if (type is IErrorTypeSymbol && expr is TupleExpressionSyntax tupleExpr)
            {
                var elemTypes = tupleExpr.Arguments
                    .Select(arg =>
                    {
                        var elemT = GetExpressionTypeSafe(arg.Expression, semanticModel);
                        return NormalizeTupleRecursive(elemT, semanticModel, arg.Expression);
                    })
                    .ToImmutableArray();

                var names = tupleExpr.Arguments
                    .Select(arg => arg.NameColon?.Name.Identifier.ValueText ?? "")
                    .ToImmutableArray();

#pragma warning disable CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
                return semanticModel.Compilation.CreateTupleTypeSymbol(elemTypes, names);
#pragma warning restore CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
            }

            // case: resolved tuple type
            // **Handle tuple**: even if Roslyn can't infer nicely, we rebuild element types:
            if (type is INamedTypeSymbol namedTuple && namedTuple.IsTupleType)
            {
                var elemTypes = namedTuple.TupleElements
                    .Select(e =>
                    {
                        var et = e.Type;
                        return NormalizeTupleRecursive(et, semanticModel, expr);
                    })
                    .ToImmutableArray();

                var names = namedTuple.TupleElements.Select(e => e.Name ?? "").ToImmutableArray();
#pragma warning disable CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
                return semanticModel.Compilation.CreateTupleTypeSymbol(elemTypes, names);
#pragma warning restore CS8620 // Argument cannot be used for parameter due to differences in the nullability of reference types.
            }

            // default: delegate to NormalizeTypeAnonymousSafe (object/anonymous…)
            return NormalizeTypeAnonymousSafe(type, semanticModel, expr);
        }

        /// <summary>
        /// Normalize a type: 
        /// - If it's an anonymous type or any generic argument is anonymous,
        ///   force the entire type to System.Object (not List<object>).
        /// - Otherwise, rebuild the type recursively (arrays, generics) but with normalized arguments.
        /// This prevents invalid generic signatures like List<<anonymous type>>.
        /// + anonymous -> object
        /// + tuple: keep tuple shape, normalize each element (anonymous/null -> object)
        /// + arrays: normalize element
        /// + generic: if any type arg is anonymous -> fallback to object (policy); otherwise reconstruct
        /// </summary>
        private static ITypeSymbol NormalizeTypeAnonymousSafe(
        ITypeSymbol? type,
        SemanticModel semanticModel,
        ExpressionSyntax? originalExpression = null)
        {
            // If null type → treat as object
            if (type is null)
                return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);

            // If direct anonymous → return object
            if (type.IsAnonymousType ||
                (type is INamedTypeSymbol namedAnon && namedAnon.IsAnonymousType))
            {
                return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);
            }

            // *** handle tuple expression with error type ***
            // (1, null, "abc")
            if (type is IErrorTypeSymbol && originalExpression is TupleExpressionSyntax tupleExpr)
            {
                return NormalizeTupleRecursive(type, semanticModel, originalExpression);
            }

            // **Handle tuple**: even if Roslyn can't infer nicely, we rebuild element types:
            if (type is INamedTypeSymbol namedTuple && namedTuple.IsTupleType)
            {
                return NormalizeTupleRecursive(type, semanticModel, originalExpression);
            }

            // If it's a generic type (List<T>, Dictionary<K,V>, etc.)
            if (type is INamedTypeSymbol named && named.TypeArguments.Length > 0)
            {
                // If ANY type argument is anonymous, 
                // we fallback the whole thing to object
                if (named.TypeArguments.Any(t =>
                    t.IsAnonymousType || (t is INamedTypeSymbol n && n.IsAnonymousType)))
                {
                    return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);
                }

                // Otherwise, rebuild generic with normalized type arguments recursively
                var newArgs = named.TypeArguments
                    .Select(t => NormalizeTypeAnonymousSafe(t, semanticModel))
                    .ToArray();

                return named.ConstructedFrom.Construct(newArgs);
            }

            // If it's an array type (including multi-dim)
            if (type is IArrayTypeSymbol arr)
            {
                // Normalize the element type first
                var elem = NormalizeTypeAnonymousSafe(arr.ElementType, semanticModel);

                // If the element became object because it was anonymous,
                // but the array itself had anonymous element → still safe to create array of object
                return semanticModel.Compilation.CreateArrayTypeSymbol(elem, arr.Rank);
            }

            // Otherwise return the type as-is
            return type;
        }

        /// <summary>
        /// Returns a simplified type symbol for multi-dimensional arrays.
        /// If the input type is a multi-dimensional array, it returns System.Object.
        /// Otherwise, it returns the original type.
        /// </summary>
        /// <param name="type">The input type symbol to analyze.</param>
        /// <param name="semanticModel">The semantic model used to access compilation information.</param>
        /// <returns>
        /// System.Object for multi-dimensional arrays; otherwise, the original type symbol.
        /// </returns>
        private static ITypeSymbol? NormalizeMultiDimensionalArrayType(ITypeSymbol? type, SemanticModel semanticModel)
        {
            if (type is IArrayTypeSymbol arrSym && arrSym.Rank > 1)
            {
                // Multi-dimensional array
                var elementType = arrSym.ElementType;

                // Manually construct the array type string
                // Example: decimal[,]
                var rankCommas = new string(',', arrSym.Rank - 1);
                var arrayName = $"{GetFriendlyTypeName(elementType)}[{rankCommas}]";

                // You could store the type as ITypeSymbol, but for multi-dimensional arrays,
                // GetFriendlyTypeName(elementType) + "[,]" or simply cast to object if deeper handling isn't needed
                // Simplest approach:
                return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);
            }
            return type;
        }

        /// <summary>
        /// get namespace
        /// </summary>
        /// <param name="type"></param>
        /// <param name="mapNamespaces"></param>
        /// <param name="namedType"></param>
        private static void GetNamespace(
        ITypeSymbol? type,
        Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces,
        INamedTypeSymbol namedType)
        {
            //if (namedType.ContainingType is not null)
            //{
            //    // skip adding using for nested type completely
            //    return;
            //}

            if (!mapNamespaces.ContainsKey(namedType))
                mapNamespaces[namedType] = new HashSet<string>();

            if (type is null)
                return;

            // handle arrays: recurse into element
            if (type is IArrayTypeSymbol arr)
            {
                GetNamespace(arr.ElementType, mapNamespaces, namedType);
                return;
            }

            // named types (including generics)
            if (type is INamedTypeSymbol named)
            {
                // Recurse into type arguments (so List<MyNs.MyEnum> will add MyNs)
                foreach (var ta in named.TypeArguments)
                    GetNamespace(ta, mapNamespaces, namedType);

                // If this is a nested type (has a containing type), DO NOT add a using for it.
                // Nested types don't have their own namespace; adding a using for "" or "<global>" is wrong.
                if (named.ContainingType is not null)
                {
                    // Optionally: we could add the containing *namespace* of the outer-most type,
                    // but to be safe we'll not add a using for nested types here.
                    return;
                }

                // For top-level named types, add their namespace (if it's not System/System.Collections.Generic)
                var ns = named.ContainingNamespace?.ToDisplayString();
                if (!string.IsNullOrEmpty(ns)
                    && !ns!.Contains("global")
                    && ns != "System"
                    && ns != "System.Collections.Generic")
                {
                    mapNamespaces[namedType].Add(ns!);
                }

                return;
            }

            // For other symbol kinds (tuple, pointer etc.) — try to be conservative:
            // e.g. if it's a tuple, inspect element types (Roslyn models tuples as INamedTypeSymbol value tuples,
            // so the above INamedTypeSymbol block already handled type arguments).
        }

        /// <summary>
        /// Helper to get string type to print to file (for all cases, nested → fully-qualified)
        /// </summary>
        /// <param name="type"></param>
        /// <param name="usingsForThisType"></param>
        /// <returns></returns>
        public static string GetTypeDisplayString(ITypeSymbol? type, HashSet<string>? usingsForThisType = null)
        {
            if (type is null)
                return "object";

            // arrays (including multi-dim)
            if (type is IArrayTypeSymbol arr)
            {
                var elem = GetTypeDisplayString(arr.ElementType, usingsForThisType);

                if (arr.Rank == 1)
                    return $"{elem}[]";

                return $"{elem}[{new string(',', arr.Rank - 1)}]";
            }

            // nullable value types handled in GetFriendlyTypeName if needed, but fall back here:
            if (type is INamedTypeSymbol named)
            {
                // If the type itself is nested (ContainingType != null) => use fully-qualified name
                if (named.ContainingType is not null)
                {
                    // Fully qualified avoids needing a using for nested types. Strip "global::"
                    return named.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat).Replace("global::", "");
                }

                // tuple type: (int, string, bool)
                if (named.IsTupleType)
                {
                    // NEW: normalize each tuple element: if anonymous or null -> object
                    // (1, null, "abc") --> (int, object, string)
                    var elements = named.TupleElements
                        .Select(e =>
                        {
                            var et = e.Type;
                            if (et is null || et.IsAnonymousType || (et is INamedTypeSymbol en && en.IsAnonymousType))
                                return "object";
                            return GetTypeDisplayString(et, usingsForThisType);
                        });
                    return $"({string.Join(", ", elements)})";
                }

                // nullable convertion: Nullable<T> -> T?
                if (named.OriginalDefinition.SpecialType == SpecialType.System_Nullable_T
                    && named.TypeArguments.Length == 1)
                {
                    var inner = GetTypeDisplayString(named.TypeArguments[0], usingsForThisType);
                    return $"{inner}?";
                }

                // If generic: need to render generic args recursively
                if (named.IsGenericType)
                {
                    // base name without namespace
                    var baseName = named.Name; // e.g. List, Dictionary
                    var args = string.Join(", ", named.TypeArguments.Select(t => GetTypeDisplayString(t, usingsForThisType)));
                    return $"{baseName}<{args}>";
                }

                // Non-generic named top-level type:
                // If we have a usings set for this generated type and it contains the namespace,
                // we can return MinimallyQualified (short) name; otherwise use MinimallyQualified anyway.
                // MinimallyQualifiedFormat will give e.g. MyEnum or MyNs.MyEnum depending on context.
                return named.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).Replace("global::", "");
            }

            // fallback: use GetFriendlyTypeName for primitives and others
            return GetFriendlyTypeName(type);
        }

        /// <summary>
        /// get all type via "new" in method body: new XXX();
        /// </summary>
        /// <param name="methodSyntax"></param>
        /// <param name="semanticModel"></param>
        /// <returns></returns>
        public static IEnumerable<ITypeSymbol> CollectCreatedTypes(
        MethodDeclarationSyntax methodSyntax,
        SemanticModel semanticModel)
        {
            // explicit new T()
            foreach (var oc in methodSyntax.DescendantNodes().OfType<ObjectCreationExpressionSyntax>())
            {
                ITypeSymbol? type = semanticModel.GetTypeInfo(oc).Type;
                if (type is not null)
                    yield return type;
            }

            // implicit new() (C# 9+)
            foreach (var io in methodSyntax.DescendantNodes().OfType<ImplicitObjectCreationExpressionSyntax>())
            {
                ITypeSymbol? type = semanticModel.GetTypeInfo(io).Type;
                if (type is not null)
                    yield return type;
            }

            // Invocation results (e.g. myList.Select(...).ToList() => IEnumerable<MyNewClass> or List<MyNewClass>)
            foreach (var inv in methodSyntax.DescendantNodes().OfType<InvocationExpressionSyntax>())
            {
                ITypeSymbol? type = semanticModel.GetTypeInfo(inv).Type;
                if (type is not null)
                    yield return type;
            }
        }

        /// <summary>
        /// collect types from variable declarations in method body: 
        /// TypeName varName = ...
        /// </summary>
        /// <param name="methodSyntax"></param>
        /// <param name="semanticModel"></param>
        /// <returns></returns>
        public static IEnumerable<ITypeSymbol> CollectDeclaredTypes(MethodDeclarationSyntax methodSyntax, SemanticModel semanticModel)
        {
            foreach (var vd in methodSyntax.DescendantNodes().OfType<VariableDeclarationSyntax>())
            {
                // If explicit type (List<MyType>), get type from vd.Type
                ITypeSymbol? type = semanticModel.GetTypeInfo(vd.Type).Type;
                if (type is not null)
                    yield return type;

                // If 'var' with initializer, get type from initializer
                if (vd.Type.IsVar)
                {
                    foreach (var v in vd.Variables)
                    {
                        if (v.Initializer?.Value is ExpressionSyntax init)
                        {
                            var it = semanticModel.GetTypeInfo(init).Type;
                            if (it != null)
                                yield return it;
                        }
                    }
                }
            }
        }

        /// <summary>
        /// generate classes as:
        /// List<Area> list1 = new List<Area>() { };  --> Area
        /// obj1.Classes  = new List<TeacherClass>() { }; --> TeacherClass
        /// </summary>
        /// <param name="methodSyntax"></param>
        /// <param name="semanticModel"></param>
        /// <returns></returns>
        public static IEnumerable<ITypeSymbol> CollectDeclaredTypesWithGenerics(
        MethodDeclarationSyntax methodSyntax,
        SemanticModel semanticModel)
        {
            // scan variable declarations in functions
            foreach (var vd in methodSyntax.DescendantNodes().OfType<VariableDeclarationSyntax>())
            {
                // // get type of the variable
                ITypeSymbol? typeInfo = semanticModel.GetTypeInfo(vd.Type).Type;
                if (typeInfo is not null && typeInfo.SpecialType == SpecialType.None)
                {
                    yield return typeInfo;

                    // If generic, take type arguments
                    if (typeInfo is INamedTypeSymbol named && named.TypeArguments.Length > 0)
                    {
                        foreach (var arg in named.TypeArguments)
                            if (arg.SpecialType == SpecialType.None)
                                yield return arg;
                    }
                }
            }

            // scan the object creation expression (new List<Area>())
            foreach (var oc in methodSyntax.DescendantNodes().OfType<ObjectCreationExpressionSyntax>())
            {
                var t = semanticModel.GetTypeInfo(oc).Type;
                if (t is not null && t.SpecialType == SpecialType.None)
                {
                    yield return t;
                    if (t is INamedTypeSymbol named && named.TypeArguments.Length > 0)
                    {
                        foreach (var arg in named.TypeArguments)
                            if (arg.SpecialType == SpecialType.None)
                                yield return arg;
                    }
                }
            }
        }

        /// <summary>
        /// Scans a <see cref="MethodDeclarationSyntax"/> for all <c>local functions</c> declared inside it
        /// and yields the <see cref="ITypeSymbol"/>s of their return types and parameter types.
        /// </summary>
        /// <remarks>
        /// This helper is used by the source generator to discover custom types
        /// that appear in local function signatures (return type or parameters),
        /// so that the generator can also generate code for those types.
        /// </remarks>
        /// <returns>
        /// An <see cref="IEnumerable{ITypeSymbol}"/> containing all distinct type symbols
        /// referenced in local function signatures within the given method.
        /// </returns>
        public static IEnumerable<ITypeSymbol> CollectLocalFunctionTypes(
            MethodDeclarationSyntax methodSyntax,
            SemanticModel semanticModel)
        {
            // Iterate through all LocalFunctionStatementSyntax nodes inside the method body
            foreach (var localFn in methodSyntax.DescendantNodes().OfType<LocalFunctionStatementSyntax>())
            {
                // Get the return type of the local function (if present)
                if (localFn.ReturnType != null)
                {
                    var returnType = semanticModel.GetTypeInfo(localFn.ReturnType).Type;
                    if (returnType != null)
                        yield return returnType;
                }

                // Get the type of each parameter in the local function
                foreach (var param in localFn.ParameterList.Parameters)
                {
                    if (param.Type != null)
                    {
                        var paramType = semanticModel.GetTypeInfo(param.Type).Type;
                        if (paramType != null)
                            yield return paramType;
                    }
                }
            }
        }

        /// <summary>
        /// unwrap List<T>, HashSet<T>... to get T
        /// </summary>
        /// <param name="type"></param>
        /// <returns></returns>
        public static INamedTypeSymbol? UnwrapCollection(ITypeSymbol? type)
        {
            if (type is null)
                return null;

            // 1) Array: Student[]
            if (type is IArrayTypeSymbol arr)
            {
                return arr.ElementType as INamedTypeSymbol;
            }

            // 2) If it's a named generic type (List<>, HashSet<>, Dictionary<,>, Queue<>, etc.)
            if (type is INamedTypeSymbol named && named.IsGenericType)
            {
                // Try to find IEnumerable<T> implemented by this type
                string ienumerableDef = "System.Collections.Generic.IEnumerable`1";
                INamedTypeSymbol? enumIface = named.AllInterfaces
                    .FirstOrDefault(i => i.OriginalDefinition?.ToDisplayString() == ienumerableDef);

                if (enumIface is not null && enumIface.TypeArguments.Length >= 1)
                {
                    ITypeSymbol element = enumIface.TypeArguments[0];

                    // If element is KeyValuePair<K,V> (Dictionary case), prefer the Value (V)
                    if (element is INamedTypeSymbol elementNamed &&
                        elementNamed.OriginalDefinition?.ToDisplayString() == "System.Collections.Generic.KeyValuePair`2")
                    {
                        if (elementNamed.TypeArguments.Length >= 2)
                        {
                            ITypeSymbol valueArg = elementNamed.TypeArguments[1];
                            return valueArg as INamedTypeSymbol ?? (elementNamed.TypeArguments[0] as INamedTypeSymbol);
                        }
                    }

                    // Else return the element type if it's a named type
                    return element as INamedTypeSymbol;
                }

                // Fallback: handle common collections by constructed-from name
                string? constructed = named.ConstructedFrom?.ToDisplayString();
                HashSet<string> oneArgCollections = new HashSet<string>
                    {
                        "System.Collections.Generic.List`1",
                        "System.Collections.Generic.HashSet`1",
                        "System.Collections.Generic.Queue`1",
                        "System.Collections.Generic.Stack`1",
                        "System.Collections.Generic.LinkedList`1",
                        "System.Collections.Generic.SortedSet`1",
                        "System.Collections.ObjectModel.ObservableCollection`1",
                        "System.Collections.Concurrent.ConcurrentBag`1",
                        "System.Collections.Concurrent.ConcurrentQueue`1"
                    };

                if (!string.IsNullOrEmpty(constructed) && oneArgCollections.Contains(constructed!))
                {
                    if (named.TypeArguments.Length >= 1)
                        return named.TypeArguments[0] as INamedTypeSymbol;
                }

                // Dictionary<,> fallback: take value type
                if (!string.IsNullOrEmpty(constructed) && constructed!.StartsWith("System.Collections.Generic.Dictionary`2"))
                {
                    if (named.TypeArguments.Length >= 2)
                        return named.TypeArguments[1] as INamedTypeSymbol;
                }

                // If nothing matched, return the named type itself
                return named;
            }

            // Not a named generic or array — return if named
            return type as INamedTypeSymbol;
        }

        [Obsolete("This method is currently unused but kept for future use.", false)]
        public static IEnumerable<INamedTypeSymbol> CollectAllNamedTypes(ITypeSymbol? type)
        {
            if (type is null)
                yield break;

            HashSet<ITypeSymbol> visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);
            Stack<ITypeSymbol> stack = new Stack<ITypeSymbol>();
            stack.Push(type);

            while (stack.Count > 0)
            {
                ITypeSymbol current = stack.Pop();
                if (current is null)
                    continue;
                if (!visited.Add(current))
                    continue; // avoid repetition

                if (current.SpecialType != SpecialType.None)
                    continue;

                // 1) Array
                if (current is IArrayTypeSymbol arr)
                {
                    stack.Push(arr.ElementType);
                    continue;
                }

                // 2) Named generic
                // if INamedTypeSymbol (generic, value tuple, simple named ...)
                if (current is INamedTypeSymbol named)
                {
                    // 2.1) Detect tuple via OriginalDefinition = System.ValueTuple`N
                    string? orig = named.OriginalDefinition?.ToDisplayString();
                    if (!string.IsNullOrEmpty(orig) && orig!.StartsWith("System.ValueTuple`", StringComparison.Ordinal))
                    {
                        // Tuple elements are usually in TypeArguments
                        foreach (ITypeSymbol ta in named.TypeArguments)
                            stack.Push(ta);
                        continue;
                    }

                    // 2.2) if IEnumerable<T>, push T 
                    string ienumerableDef = "System.Collections.Generic.IEnumerable`1";
                    INamedTypeSymbol? enumIface = named.AllInterfaces.FirstOrDefault(i =>
                        string.Equals(i.OriginalDefinition?.ToDisplayString(), ienumerableDef, StringComparison.Ordinal));

                    if (enumIface is not null && enumIface.TypeArguments.Length >= 1)
                    {
                        ITypeSymbol element = enumIface.TypeArguments[0];

                        // if element is KeyValuePair<K,V>, prefer V (value) to generate
                        if (element is INamedTypeSymbol elemNamed &&
                            elemNamed.OriginalDefinition?.ToDisplayString() == "System.Collections.Generic.KeyValuePair`2" &&
                            elemNamed.TypeArguments.Length >= 2)
                        {
                            stack.Push(elemNamed.TypeArguments[1]); // push Value type
                        }
                        else
                        {
                            stack.Push(element);
                        }

                        // also push generic args to dig deeper
                        if (named.IsGenericType)
                        {
                            foreach (ITypeSymbol arg in named.TypeArguments)
                                stack.Push(arg);
                        }

                        // If no system namespace then yield this named type as candidate
                        if (!named.ContainingNamespace?.ToDisplayString().StartsWith("System") ?? true)
                            yield return named;

                        continue;
                    }

                    // 2.3) If it is a different generic (e.g. Option<T>, MyWrapper<T>), push type args
                    if (named.IsGenericType)
                    {
                        foreach (ITypeSymbol arg in named.TypeArguments)
                            stack.Push(arg);
                    }

                    // If named is not in System.* then add it as a candidate (e.g. Student)
                    string ns = named.ContainingNamespace?.ToDisplayString() ?? "";
                    if (!ns.StartsWith("System", StringComparison.Ordinal))
                    {
                        yield return named;
                    }

                    continue;
                }
            }
        }

        /// <summary>
        /// return the return type of the method, and also types of all return statements inside the method body.
        /// </summary>
        /// <param name="method"></param>
        /// <param name="compilation"></param>
        /// <returns></returns>
        public static IEnumerable<ITypeSymbol> FindReturnedTypes(MethodDeclarationSyntax method, Compilation compilation)
        {
            SemanticModel semanticModel = compilation.GetSemanticModel(method.SyntaxTree);
            IMethodSymbol? methodSymbol = semanticModel.GetDeclaredSymbol(method);
            if (methodSymbol is null)
                return Enumerable.Empty<ITypeSymbol>();

            ITypeSymbol returnType = methodSymbol.ReturnType;

            HashSet<ITypeSymbol> result = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

            // ignore void, Task, not generic
            if (returnType.SpecialType != SpecialType.System_Void)
            {
                // unwrap Task<T> / ValueTask<T>
                if (returnType is INamedTypeSymbol named && named.IsGenericType &&
                    (named.Name == "Task" || named.Name == "ValueTask"))
                {
                    ITypeSymbol inner = named.TypeArguments[0];
                    if (inner.SpecialType != SpecialType.System_Void)
                        result.Add(inner);
                }
                else
                {
                    result.Add(returnType);
                }
            }

            // Scan return statements in function body
            foreach (ReturnStatementSyntax ret in method.DescendantNodes().OfType<ReturnStatementSyntax>())
            {
                if (ret.Expression is null)
                    continue;

                //Microsoft.CodeAnalysis.TypeInfo typeInfo = semanticModel.GetTypeInfo(ret.Expression);
                //ITypeSymbol? rawType = typeInfo.Type;
                var rawType = GetExpressionTypeSafe(ret.Expression, semanticModel);

                var normType = NormalizeTypeAnonymousSafe(rawType, semanticModel, ret.Expression);
                if (normType is not null && normType.SpecialType != SpecialType.System_Void)
                {
                    result.Add(normType);
                }
            }

            return result;
        }

        /// <summary>
        /// use to check if a type is already defined in the compilation (has a source declaration)
        /// </summary>
        /// <param name="typeSym"></param>
        /// <param name="compilation"></param>
        /// <returns></returns>
        public static bool TypeDefinedInCompilation(INamedTypeSymbol typeSym, Compilation compilation)
        {
            // If any location is in the compilation source tree => already exists
            foreach (Location loc in typeSym.Locations)
            {
                if (loc.IsInSource)
                    return true;
            }

            // or use GetSymbolsWithName:
            bool existing = compilation.GetSymbolsWithName(n => n == typeSym.Name)
                .OfType<INamedTypeSymbol>()
                .Any(sym => sym.Name == typeSym.Name && sym.Locations.Any(l => l.IsInSource));

            return existing;
        }

        /// <summary>
        /// Generates C# class source code based on the provided type name, properties, and configuration.
        /// Supports generic type parameters, extra usings, and constructors.
        /// Skips generation for System.* types.
        /// </summary>
        /// <param name="typeName">The name of the class to generate.</param>
        /// <param name="properties">A collection of properties with their names and types.</param>
        /// <param name="config">Configuration options for class generation.</param>
        /// <returns>A string containing the generated class source code.</returns>
        public static string GenerateClassSource(
        INamedTypeSymbol namedTypeSymbol,
        IEnumerable<(string PropName, string PropType)> properties,
        GenerateClassOption config,
        Dictionary<INamedTypeSymbol, List<(string CtorKey,
                                   List<(string? Name, ITypeSymbol Type)> Params,
                                   bool IsNamedArgs)>> mapCtor,
        Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces,
        SourceProductionContext spc)
        {
            if (config is null)
            {
                throw new ArgumentNullException(nameof(config));
            }

            // Build code string; include "using System" and System.Collections.Generic if needed.
            // Use config.Namespace if provided, else use "Generated".
            string namespaceOfFile = string.IsNullOrWhiteSpace(config!.Namespace) ? "Generated" : config.Namespace!;

            // Convert RequiredProperties to Dictionary<string,string>
            var requiredProperties =
                (config.RequiredProperties ?? new Dictionary<string, object>())
                .ToDictionary(
                    kv => kv.Key,
                    kv =>
                    {
                        if (kv.Value is string s) return s;
                        if (kv.Value is Type t) return GetSimpleTypeName(t);
                        if (kv.Value is ITypeSymbol sym) return GetFriendlyTypeName(sym);
                        return "object";
                    });

            // Merge properties from dev config and discovered properties
            List<(int proIndex, string PropName, string PropType)> allProps =
                requiredProperties
                    .Select(kv => (proIndex: int.MinValue, PropName: kv.Key, PropType: kv.Value))
                    .Concat(
                        properties.Where(p => !requiredProperties.ContainsKey(p.PropName))
                                  .Select(p => (proIndex: int.MaxValue, p.PropName, p.PropType))
                    )
                    .OrderBy(p => p.proIndex)
                    .ToList();

            // 1. Generate "using namespace"
            List<string> namespaces = new List<string>() { };

            if (mapNamespaces.TryGetValue(namedTypeSymbol, out var nsList))
            {
                foreach (var ns in nsList)
                {
                    namespaces.Add(ns);
                }
            }

            namespaces = namespaces.Distinct().OrderBy(n => n).ToList();

            StringBuilder sb = new StringBuilder();
            foreach (var ns in namespaces)
            {
                sb.AppendLine($"using {ns};");
            }
            if (namespaces.Count > 0)
                sb.AppendLine();
            sb.AppendLine($"namespace {namespaceOfFile}");
            sb.AppendLine("{");

            // 2. Generate class
            // public class MyClass
            // public class MyClass<T>
            // public class MyClass<T1, T2>
            var def = namedTypeSymbol.OriginalDefinition;
            var typeName = def.Name;
            var paramNames = def.TypeParameters.Select(tp => tp.Name).ToList();
            if (paramNames.Count == 0 && namedTypeSymbol.TypeArguments.Length > 0)
            {
                paramNames = Enumerable.Range(1, namedTypeSymbol.TypeArguments.Length)
                                       .Select(i => $"T{i}")
                                       .ToList();
            }
            if (paramNames.Count > 0)
            {
                paramNames = Enumerable.Range(1, paramNames.Count)
                                       .Select(i => string.IsNullOrWhiteSpace(paramNames[i - 1])
                                                    ? $"T{i}"
                                                    : paramNames[i - 1])
                                       .ToList();
                typeName += $"<{string.Join(", ", paramNames)}>";
            }

            sb.AppendLine($"\tpublic partial class {typeName}");
            sb.AppendLine("\t{");

            // 3. Generate properties
            foreach ((int proIndex, string PropName, string PropType) prop in allProps)
            {
                sb.AppendLine($"\t\t/// <summary>");
                sb.AppendLine($"\t\t/// {prop.PropName}");
                sb.AppendLine($"\t\t/// </summary>");
                sb.AppendLine($"\t\tpublic {prop.PropType} {prop.PropName} {{ get; set; }}\n");
            }

            // 4. Generate constructors
            if (mapCtor.TryGetValue(namedTypeSymbol, out var allCtor))
            {
                int numberOfCtor = allCtor.Count;
                for (int idx = 0; idx < numberOfCtor; idx++)
                {
                    var ctor = allCtor[idx];

                    var signature = string.Join(", ",
                                ctor.Params.Select((p, i) =>
                                {
                                    var paramName = !string.IsNullOrWhiteSpace(p.Name)
                                                        ? p.Name
                                                        : $"property{i}";
                                    return $"{GetFriendlyTypeName(p.Type)} {paramName}";
                                }));

                    sb.AppendLine($"\t\tpublic {namedTypeSymbol.Name}({signature})");
                    sb.AppendLine("\t\t{");
                    sb.AppendLine("\t\t}");

                    if (idx < numberOfCtor - 1)
                        sb.AppendLine();
                }
            }

            sb.AppendLine("\t}");
            sb.AppendLine("}");

            return sb.ToString();
        }

        /// <summary>
        /// check if any property type starts with a known collection type prefix
        /// (Check if any property type indicates a collection type that needs System.Collections.Generic
        /// </summary>
        /// <param name="props"></param>
        [Obsolete("This method is currently unused but kept for future use.", false)]
        public static bool NeedsCollectionsUsing(IEnumerable<(int proIndex, string PropName, string PropType)> props)
        {
            string[] prefixes =
            {
                    "List<", "HashSet<", "Dictionary<", "Queue<", "Stack<",
                    "LinkedList<", "SortedSet<", "ObservableCollection<",
                    "ConcurrentBag<", "ConcurrentQueue<"
            };

            return props.Any(p => prefixes.Any(pref =>
                p.PropType.StartsWith(pref, StringComparison.Ordinal)));
        }

        [Obsolete("This method is currently unused but kept for future use.", false)]
        public static void WriteLog(SourceProductionContext spc, object content, string? id = null)
        {
            StringBuilder sb = new StringBuilder();
            var dt = DateTime.Now.ToString("dd-MM-yyyy_HH-mm-ss-ffffff");
            sb.AppendLine($"[New][{dt}]");
            sb.AppendLine(content.ToString());
            sb.AppendLine();
            if (string.IsNullOrEmpty(id))
                spc.AddSource($"Logs_{dt}_{Guid.NewGuid()}.log", sb.ToString());
            else
                spc.AddSource($"Logs_{id}_{dt}_{Guid.NewGuid()}.log", sb.ToString());
        }
    }
}
