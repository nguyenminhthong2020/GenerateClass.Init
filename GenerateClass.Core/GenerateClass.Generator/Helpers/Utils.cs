using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.DependencyInjection;
using System.Reflection;
using System.Text;

namespace GenerateClass.Generator.Helpers
{
    public static class Utils
    {
        /// <summary>
        /// 
        /// </summary>
        /// <param name="expr"></param>
        /// <param name="sm"></param>
        /// <returns></returns>
        public static string? TryGetStringFromExpression(ExpressionSyntax? expr, SemanticModel sm)
        {
            if (expr is null) return null;

            // string literal "abc"
            if (expr is LiteralExpressionSyntax lit)
                return lit.Token.ValueText;

            // typeof(int) => "int"
            if (expr is TypeOfExpressionSyntax tos)
            {
                var tSym = sm.GetTypeInfo(tos.Type).Type;
                if (tSym is not null)
                    return FriendlyTypeName(tSym);
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

        //public static string FriendlyTypeName(ITypeSymbol tSym)
        //{
        //    // Với alias C# cơ bản
        //    switch (tSym.SpecialType)
        //    {
        //        case SpecialType.System_Int32: return "int";
        //        case SpecialType.System_Int64: return "long";
        //        case SpecialType.System_String: return "string";
        //        case SpecialType.System_Boolean: return "bool";
        //            // thêm các alias khác nếu cần
        //    }

        //    // generic type: List<Student>
        //    if (tSym is INamedTypeSymbol named && named.IsGenericType)
        //    {
        //        var name = named.Name; // List
        //        var args = string.Join(", ", named.TypeArguments.Select(FriendlyTypeName));
        //        return $"{name}<{args}>";
        //    }

        //    // mặc định trả full name
        //    return tSym.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat)
        //               .Replace("global::", "");
        //}

        public static string FriendlyTypeName(ITypeSymbol t)
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
                case SpecialType.System_Int32: return "int";
                case SpecialType.System_Int64: return "long";
                case SpecialType.System_String: return "string";
                case SpecialType.System_Object: return "object";
            }

            // nullable
            if (t is INamedTypeSymbol namedNullable &&
                namedNullable.ConstructedFrom.SpecialType == SpecialType.System_Nullable_T)
            {
                var inner = namedNullable.TypeArguments[0];
                return FriendlyTypeName(inner) + "?";
            }

            // array
            // multi-dimensional array, jagged array
            if (t is IArrayTypeSymbol arr)
            {
                var elem = FriendlyTypeName(arr.ElementType);
                if (arr.Rank == 1)
                    return elem + "[]";
                else
                    return elem + "[" + new string(',', arr.Rank - 1) + "]";
            }

            // generic type: List<Student>
            if (t is INamedTypeSymbol named && named.IsGenericType)
            {
                var name = named.Name; // List, Dictionary, etc
                var args = string.Join(", ", named.TypeArguments.Select(FriendlyTypeName));
                return $"{name}<{args}>";
            }

            // nullable reference types: string?, List<Student>?
            // (C# 8+ with #nullable enable)
            if (t.NullableAnnotation == NullableAnnotation.Annotated && !t.IsValueType)
            {
                var bare = t.WithNullableAnnotation(NullableAnnotation.None);
                return FriendlyTypeName(bare) + "?";
            }

            // fallback to minimally qualified (e.g. MyNamespace.MyType)
            return t.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat).Replace("global::", "");
        }

        /// <summary>
        /// Find object initializers and assignments in the method body to get the properties assigned to this type.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="methodSyntax"></param>
        /// <param name="compilation"></param>
        /// <param name="map"></param>
        /// <param name="methodSymbol"></param>
        public static void CollectTypeAndMembers(
        ITypeSymbol type,
        MethodDeclarationSyntax methodSyntax,
        Compilation compilation,
        //Dictionary<INamedTypeSymbol, HashSet<string>> map,
        Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> map,
        Dictionary<INamedTypeSymbol, List<(string Name, ITypeSymbol Type)>> mapCtor,
        Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces,
        IMethodSymbol methodSymbol)
        {
            INamedTypeSymbol? namedType = UnwrapCollection(type) ?? type as INamedTypeSymbol;
            if (namedType is null)
                return;

            //if (!map.ContainsKey(namedType))
            //    map[namedType] = new HashSet<string>();
            if (!map.ContainsKey(namedType))
                map[namedType] = new Dictionary<string, ITypeSymbol?>();

            // Find object initializers in the method body that match this type:
            SemanticModel semanticModel = compilation.GetSemanticModel(methodSyntax.SyntaxTree);

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
                    if (creation.Initializer is not null)
                    {
                        foreach (ExpressionSyntax expr in creation.Initializer.Expressions)
                        {
                            if (expr is AssignmentExpressionSyntax assign)
                            {
                                // use semantic model to get property name correctly
                                var leftSym = semanticModel.GetSymbolInfo(assign.Left).Symbol as IPropertySymbol;
                                // string propName = leftSym?.Name ?? assign.Left.ToString().Split('.').Last();
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

                                // map[namedType].Add(propName);
                                ITypeSymbol? rightType = semanticModel.GetTypeInfo(assign.Right).Type;
                                rightType = NormalizeTypeAnonymousSafe(rightType, semanticModel);
                                rightType = NormalizeMultiDimensionalArrayType(rightType, semanticModel);
                                map[namedType][propName] = rightType;
                                GetNamespace(rightType, mapNamespaces, namedType);
                            }
                        }
                    }

                    // var obj = new MyObject(Prop1: val1, Prop2: val2,...)
                    if (creation.ArgumentList is not null)
                    {
                        var ctorParams = new List<(string Name, ITypeSymbol Type)>();

                        foreach (var arg in creation.ArgumentList.Arguments)
                        {
                            if (arg.NameColon is not null)
                            {
                                string propName = arg.NameColon.Name.Identifier.Text;
                                ITypeSymbol? argType = semanticModel.GetTypeInfo(arg.Expression).Type;
                                argType = NormalizeTypeAnonymousSafe(argType, semanticModel);
                                argType = NormalizeMultiDimensionalArrayType(argType, semanticModel);
                                map[namedType][propName] = argType;

                                ctorParams.Add((propName, argType!));
                                GetNamespace(argType, mapNamespaces, namedType);
                            }
                        }

                        if (ctorParams.Count > 0)
                        {
                            mapCtor[namedType] = ctorParams;
                        }
                    }
                }
            }

            // Also, look for separate assignments to obj.Prop = ...;
            IEnumerable<AssignmentExpressionSyntax> assignments = methodSyntax.DescendantNodes()
                                                                    .OfType<AssignmentExpressionSyntax>();
            foreach (AssignmentExpressionSyntax assign in assignments)
            {
                //ISymbol? leftSymbol = semanticModel.GetSymbolInfo(assign.Left).Symbol;
                //if (leftSymbol is IPropertySymbol propSym &&
                //    propSym.ContainingType is not null &&
                //    SymbolEqualityComparer.Default.Equals(UnwrapCollection(propSym.ContainingType), namedType))
                //{
                //    // map[namedType].Add(propSym.Name);

                //    var rightType = semanticModel.GetTypeInfo(assign.Right).Type;
                //    map[namedType][propSym.Name] = rightType;
                //}

                ISymbol? leftSymbol = semanticModel.GetSymbolInfo(assign.Left).Symbol;
                string propName;

                // prioritize symbol.Name, otherwise parse syntax
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
                    var exprType = semanticModel.GetTypeInfo(ma.Expression).Type;
                    if (!SymbolEqualityComparer.Default.Equals(UnwrapCollection(exprType), namedType))
                        continue;

                    propName = ma.Name.Identifier.Text;
                }
                else
                {
                    // fallback — but must check type first
                    var exprType = semanticModel.GetTypeInfo(assign.Left).Type;
                    if (!SymbolEqualityComparer.Default.Equals(UnwrapCollection(exprType), namedType))
                        continue;

                    propName = assign.Left.ToString().Split('.').Last();
                }

                ITypeSymbol? rightType = semanticModel.GetTypeInfo(assign.Right).Type;
                rightType = NormalizeTypeAnonymousSafe(rightType, semanticModel);
                rightType = NormalizeMultiDimensionalArrayType(rightType, semanticModel);
                map[namedType][propName] = rightType;
                GetNamespace(rightType, mapNamespaces, namedType);
            }
        }

        /// <summary>
        /// Returns the original type if it is not an anonymous type; 
        /// otherwise returns System.Object as a fallback.
        /// This is useful when generating code because anonymous types 
        /// cannot be directly represented in generated classes.
        /// </summary>
        private static ITypeSymbol? EnsureNonAnonymousType(ITypeSymbol? type, SemanticModel semanticModel)
        {
            if (type is null
                || type.IsAnonymousType
                || (type is INamedTypeSymbol named && named.IsAnonymousType))
            {
                // Force fallback to object for anonymous or null types
                return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);
            }

            return type;
        }

        /// <summary>
        /// Normalize a type: 
        /// - If it's an anonymous type or any generic argument is anonymous,
        ///   force the entire type to System.Object (not List<object>).
        /// - Otherwise, rebuild the type recursively (arrays, generics) but with normalized arguments.
        /// This prevents invalid generic signatures like List<<anonymous type>>.
        /// </summary>
        private static ITypeSymbol NormalizeTypeAnonymousSafe(ITypeSymbol? type, SemanticModel semanticModel)
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
                var arrayName = $"{FriendlyTypeName(elementType)}[{rankCommas}]";

                // You could store the type as ITypeSymbol, but for multi-dimensional arrays,
                // FriendlyTypeName(elementType) + "[,]" or simply cast to object if deeper handling isn't needed
                // Simplest approach:
                return semanticModel.Compilation.GetSpecialType(SpecialType.System_Object);
            }
            return type;
        }

        //private static void GetNamespace(
        //ITypeSymbol? type,
        //Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces,
        //INamedTypeSymbol namedType)
        //{
        //    if (!mapNamespaces.ContainsKey(namedType))
        //        mapNamespaces[namedType] = new HashSet<string>();

        //    if (type is not null)
        //    {
        //        var ns = type.ContainingNamespace?.ToDisplayString();
        //        if (!string.IsNullOrEmpty(ns)
        //            && ns != "System"
        //            && ns != "System.Collections.Generic")
        //        {
        //            // gom namespace lại để cuối cùng thêm using
        //            if (!mapNamespaces[namedType].Contains(ns!))
        //                mapNamespaces[namedType].Add(ns!);
        //        }
        //    }
        //}

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

            // nullable value types handled in FriendlyTypeName if needed, but fall back here:
            if (type is INamedTypeSymbol named)
            {
                // If the type itself is nested (ContainingType != null) => use fully-qualified name
                if (named.ContainingType is not null)
                {
                    // Fully qualified avoids needing a using for nested types. Strip "global::"
                    return named.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat).Replace("global::", "");
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

            // fallback: use FriendlyTypeName for primitives and others
            return FriendlyTypeName(type);
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
            //return methodSyntax.DescendantNodes()
            //    .OfType<ObjectCreationExpressionSyntax>()
            //    .Select(oc => semanticModel.GetTypeInfo(oc).Type)
            //    .Where(t => t is not null && t.SpecialType == SpecialType.None)!;


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
            //return methodSyntax.DescendantNodes()
            //    .OfType<VariableDeclarationSyntax>()
            //    .Select(v => semanticModel.GetTypeInfo(v.Type).Type)
            //    .Where(t => t is not null && t.SpecialType == SpecialType.None)!;

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

                Microsoft.CodeAnalysis.TypeInfo typeInfo = semanticModel.GetTypeInfo(ret.Expression);
                ITypeSymbol? t = typeInfo.Type;
                if (t is not null && t.SpecialType != SpecialType.System_Void)
                    result.Add(t);
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
        /// </summary>
        /// <param name="typeName">The name of the class to generate.</param>
        /// <param name="properties">A collection of properties with their names and types.</param>
        /// <param name="config">Configuration options for class generation.</param>
        /// <returns>A string containing the generated class source code.</returns>
        public static string GenerateClassSource(
        INamedTypeSymbol namedTypeSymbol,
        IEnumerable<(string PropName, string PropType)> properties,
        GenerateClassOption config,
        Dictionary<INamedTypeSymbol, List<(string Name, ITypeSymbol Type)>> mapCtor,
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

            // Merge properties from dev config and discovered properties
            List<(string PropName, string PropType)> allProps = new List<(string PropName, string PropType)>();
            if (config.RequiredProperties is not null)
            {
                foreach (KeyValuePair<string, string> kv in config.RequiredProperties)
                {
                    // Avoid duplicate property names
                    if (!properties.Any(p => p.PropName == kv.Key))
                        allProps.Add((kv.Key, kv.Value));
                }
            }
            allProps.AddRange(properties);

            //StringBuilder sb1 = new StringBuilder();
            //var dt = DateTime.Now.ToString("dd-MM-yyyy_HH-mm-ss-ffffff");
            //sb1.AppendLine($"[New][{dt}]");
            //sb1.AppendLine("\t1. allProps (" + typeName + "):\n");
            //sb1.AppendLine(string.Join("\n\t", allProps.Select(x => x.Item1 + ":" + x.Item2)));
            //sb1.AppendLine();
            //spc.AddSource($"Log_{typeName}_{dt}.txt", sb1.ToString());

            List<string> namespaces = new List<string>() { "System" };

            if (NeedsCollectionsUsing(allProps))
            {
                namespaces.Add("System.Collections.Generic");
            }

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

            sb.AppendLine();
            sb.AppendLine($"namespace {namespaceOfFile}");
            sb.AppendLine("{");
            sb.AppendLine($"\tpublic class {namedTypeSymbol.Name}");
            sb.AppendLine("\t{");
            foreach ((string PropName, string PropType) prop in allProps)
            {
                sb.AppendLine($"\t\t/// <summary>");
                sb.AppendLine($"\t\t/// {prop.PropName}");
                sb.AppendLine($"\t\t/// </summary>");
                sb.AppendLine($"\t\tpublic {prop.PropType} {prop.PropName} {{ get; set; }}\n");
            }

            if (mapCtor.TryGetValue(namedTypeSymbol, out var ctorParams))
            {
                // generate ctor signature:
                var ctorSignature = string.Join(", ", ctorParams.Select(p => $"{p.Type.ToDisplayString()} {p.Name}"));
                sb.AppendLine($"\t\tpublic {namedTypeSymbol.Name}({ctorSignature})");
                sb.AppendLine("\t\t{");
                foreach (var p in ctorParams)
                {
                    sb.AppendLine($"\t\t\tthis.{p.Name} = {p.Name};");
                }
                sb.AppendLine("\t\t}");
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
        /// <returns></returns>
        public static bool NeedsCollectionsUsing(IEnumerable<(string PropName, string PropType)> props)
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
    }
}
