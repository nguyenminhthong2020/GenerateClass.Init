using GenerateClass.Generator.Data;
using GenerateClass.Generator.Enums;
using GenerateClass.Generator.Helpers;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.Extensions.DependencyInjection;

namespace GenerateClass.Generator
{
    [Generator]
    public class GenerateClassIncrementalGenerator : IIncrementalGenerator
    {
        private const string GenerateClassAttribute = @"GenerateClass.Generator.Attributes.GenerateClassAttribute";
        private const string AddGenerateClassMethod = "AddGenerateClass";
        private string GeneratedFileName = "{0}.cs";

        public void Initialize(IncrementalGeneratorInitializationContext context)
        {
            // 1) Find all methods annotated with GenerateClassAttribute
            var methodsWithAttr = context.SyntaxProvider
                .ForAttributeWithMetadataName(
                    GenerateClassAttribute,
                    (node, ct) => node is MethodDeclarationSyntax,
                    (ctx, ct) =>
                    {
                        MethodDeclarationSyntax methodSyntax = (MethodDeclarationSyntax)ctx.TargetNode!;
                        IMethodSymbol methodSymbol = (IMethodSymbol)ctx.SemanticModel.GetDeclaredSymbol(methodSyntax, ct)!;
                        AttributeData? attributeData = methodSymbol.GetAttributes()
                            .FirstOrDefault(ad => ad.AttributeClass?.ToDisplayString() == GenerateClassAttribute);
                        if (attributeData is null) return null;

                        // read enum args (Action, Mode) from attribute (they are compile-time constants)
                        int mode = attributeData.ConstructorArguments.Length >= 2
                            ? (int)attributeData.ConstructorArguments[1].Value! : (int)(Mode.All);

                        return new
                        {
                            MethodSymbol = methodSymbol,
                            MethodSyntax = methodSyntax,
                            Mode = (Mode)mode
                        };
                    })
                .Where(x => x is not null);

            // 2) Find AddGenerateClass(...) invocations to extract config (we only support simple literal initializers)
            IncrementalValueProvider<System.Collections.Immutable.ImmutableArray<GenerateClassOption?>> addGenConfig = context.SyntaxProvider.CreateSyntaxProvider(
                predicate: (node, ct) =>
                {
                    if (node is InvocationExpressionSyntax inv)
                    {
                        ExpressionSyntax expr = inv.Expression;
                        string name = expr.ToString();
                        return name.Contains(AddGenerateClassMethod);
                    }
                    return false;
                },
                transform: (ctx, ct) =>
                {
                    InvocationExpressionSyntax inv = (InvocationExpressionSyntax)ctx.Node;
                    // try to find lambda argument
                    ArgumentSyntax? arg = inv.ArgumentList.Arguments.FirstOrDefault();
                    if (arg?.Expression is SimpleLambdaExpressionSyntax lambda)
                    {
                        // parse assignments inside the lambda body (should be block)
                        GenerateClassOption opts = new GenerateClassOption();
                        if (lambda.Body is BlockSyntax block)
                        {
                            foreach (StatementSyntax stmt in block.Statements)
                            {
                                if (stmt is ExpressionStatementSyntax es && es.Expression is AssignmentExpressionSyntax assign)
                                {
                                    // left should be MemberAccessExpression e.g. config.Path
                                    if (assign.Left is MemberAccessExpressionSyntax left)
                                    {
                                        string name = left.Name.Identifier.Text;
                                        if (name == "Path" && assign.Right is LiteralExpressionSyntax lit)
                                            opts.Path = lit.Token.ValueText;
                                        else if (name == "Enabled" && assign.Right is LiteralExpressionSyntax bl)
                                            opts.Enabled = bl.IsKind(SyntaxKind.TrueLiteralExpression);
                                        else if (name == "Namespace" && assign.Right is LiteralExpressionSyntax ns)
                                            opts.Namespace = ns.Token.ValueText;
                                        else if (name == "RequiredProperties")
                                        {
                                            // Ensure dictionary exists
                                            opts.RequiredProperties ??= new Dictionary<string, string>();

                                            // get initializer for either explicit "new X { ... }" or implicit "new() { ... }"
                                            InitializerExpressionSyntax? initializer = null;

                                            if (assign.Right is ObjectCreationExpressionSyntax oce)
                                                initializer = oce.Initializer;
                                            else if (assign.Right is ImplicitObjectCreationExpressionSyntax ioce)
                                                initializer = ioce.Initializer;
                                            else if (assign.Right is InitializerExpressionSyntax directInit) // sometimes direct assignment to initializer
                                                initializer = directInit;

                                            if (initializer is not null)
                                            {
                                                foreach (ExpressionSyntax element in initializer.Expressions)
                                                {
                                                    // Case A: inner brace pair { "Id", "long" }
                                                    if (element is InitializerExpressionSyntax pair && pair.Expressions.Count == 2)
                                                    {
                                                        var keyExpr = pair.Expressions[0];
                                                        var valExpr = pair.Expressions[1];

                                                        string? key = (keyExpr as LiteralExpressionSyntax)?.Token.ValueText
                                                                      ?? Utils.TryGetStringFromExpression(keyExpr, ctx.SemanticModel);

                                                        string? val = (valExpr as LiteralExpressionSyntax)?.Token.ValueText;

                                                        if (val is null)
                                                        {
                                                            // handle typeof(...) form
                                                            if (valExpr is TypeOfExpressionSyntax tos)
                                                            {
                                                                var tSym = ctx.SemanticModel.GetTypeInfo(tos.Type).Type;
                                                                if (tSym is not null) val = Utils.GetFriendlyTypeName(tSym);
                                                            }
                                                            else
                                                            {
                                                                // fallback: try to eval identifier/const
                                                                val = Utils.TryGetStringFromExpression(valExpr, ctx.SemanticModel);
                                                            }
                                                        }

                                                        if (!string.IsNullOrEmpty(key) && !string.IsNullOrEmpty(val))
                                                            opts.RequiredProperties[key] = val;
                                                    }
                                                    // Case B: index initializer form ["Id"] = typeof(long) -> represented as an AssignmentExpressionSyntax inside initializer
                                                    else if (element is AssignmentExpressionSyntax idxAssign)
                                                    {
                                                        // left may be ElementAccessExpressionSyntax or an implicit form - avoid referencing Roslyn-specific implicit node.
                                                        string? key = null;

                                                        ExpressionSyntax leftExpr = idxAssign.Left;
                                                        ExpressionSyntax? keyExpr = null;

                                                        // 1) Common case: explicit element access like dict["Id"]
                                                        if (leftExpr is ElementAccessExpressionSyntax eae)
                                                        {
                                                            keyExpr = eae.ArgumentList.Arguments.FirstOrDefault()?.Expression;
                                                        }
                                                        else
                                                        {
                                                            // 2) Generic fallback: some nodes (including implicit element access in newer Roslyn)
                                                            //    have a BracketedArgumentListSyntax as a child — try to find it.
                                                            var bracket = leftExpr.ChildNodes().OfType<BracketedArgumentListSyntax>().FirstOrDefault();
                                                            if (bracket is not null)
                                                                keyExpr = bracket.Arguments.FirstOrDefault()?.Expression;
                                                        }

                                                        if (keyExpr is not null)
                                                        {
                                                            key = (keyExpr as LiteralExpressionSyntax)?.Token.ValueText
                                                                  ?? Utils.TryGetStringFromExpression(keyExpr, ctx.SemanticModel)
                                                                  ?? keyExpr.ToString().Trim('"');
                                                        }
                                                        else
                                                        {
                                                            // final fallback: extract text between [ and ]
                                                            var txt = idxAssign.Left.ToString();
                                                            int a = txt.IndexOf('[');
                                                            int b = txt.LastIndexOf(']');
                                                            if (a >= 0 && b > a)
                                                                key = txt.Substring(a + 1, b - a - 1).Trim().Trim('"');
                                                            else
                                                                key = txt.Trim().Trim('"');
                                                        }

                                                        // value side
                                                        var valueSide = idxAssign.Right;
                                                        string? val = (valueSide as LiteralExpressionSyntax)?.Token.ValueText;
                                                        if (val is null)
                                                        {
                                                            if (valueSide is TypeOfExpressionSyntax tos2)
                                                            {
                                                                var tSym2 = ctx.SemanticModel.GetTypeInfo(tos2.Type).Type;
                                                                if (tSym2 is not null) val = Utils.GetFriendlyTypeName(tSym2);
                                                            }
                                                            else
                                                            {
                                                                val = Utils.TryGetStringFromExpression(valueSide, ctx.SemanticModel) ?? valueSide.ToString().Trim('"');
                                                            }
                                                        }

                                                        if (!string.IsNullOrEmpty(key) && !string.IsNullOrEmpty(val))
                                                            opts.RequiredProperties[key] = val;
                                                    }
                                                } // foreach initializer element
                                            } // initializer is not null
                                        }

                                    }
                                }
                            }
                        }
                        return opts;
                    }
                    return null;
                })
                .Where(x => x is not null)
                .Collect(); // might be multiple projects; we pick the first

            // 3) Combine method targets + config + compilation and generate
            var combined = context.CompilationProvider.Combine(methodsWithAttr.Collect()).Combine(addGenConfig);

            //if (!System.Diagnostics.Debugger.IsAttached)
            //    System.Diagnostics.Debugger.Launch();

            context.RegisterSourceOutput(combined, (spc, source) =>
            {
                // Test 
                spc.ReportDiagnostic(Diagnostic.Create(
                new DiagnosticDescriptor(
                    id: "SG001",
                    title: "GenerateClass,Generator Start",
                    messageFormat: "GenerateClass.Generator is running!!!",
                    category: "GenerateClass.Generator Start",
                    DiagnosticSeverity.Warning,
                    isEnabledByDefault: true),
                Location.None));

#pragma warning disable CS8619 // Nullability of reference types in value doesn't match target type.
                ((Compilation compilation, var methods), System.Collections.Immutable.ImmutableArray<GenerateClassOption> configs) = source;
#pragma warning restore CS8619 // Nullability of reference types in value doesn't match target type.
                GenerateClassOption config = configs.FirstOrDefault() ?? new GenerateClassOption();

                if (!(config.Enabled ?? true))
                    return;

                // aggregated map across all methods: TypeSymbol -> (PropName -> ITypeSymbol?)
                Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> aggregated
                                              = new(SymbolEqualityComparer.Default);
                HashSet<string> usedHintNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                Dictionary<(string nspace, string namedTypeSymbol),
                          (INamedTypeSymbol typeSymbol,
                          List<(string PropName, string PropType)> propList,
                          Dictionary<INamedTypeSymbol, List<(string CtorKey, List<(string Name, ITypeSymbol Type)> Params)>> mapCtor,
                          Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces)>
                   mapClassesToGenerate = new();

                // For each method with attribute:
                foreach (var item in methods)
                {
                    if (item is null)
                        continue;

                    if (item.Mode == Mode.None)
                        continue;

                    // key = typeSymbol (Teacher), value = map propName -> ITypeSymbol? (kiểu lấy từ RHS nếu có)
                    Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> typesToGenerate
                             = new(SymbolEqualityComparer.Default);

                    // var obj = new MyObject(Prop1: val1, Prop2: val2,...)
                    // Ctor: constructor symbol → collect param names and types
                    Dictionary<INamedTypeSymbol, List<(string CtorKey, List<(string Name, ITypeSymbol Type)> Params)>> mapCtor
                             = new(SymbolEqualityComparer.Default);

                    // Namespace imports to add to generated files
                    Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces = new();

                    // Collect based on Mode:
                    if (item.Mode == Mode.Parameter || item.Mode == Mode.All)
                    {
                        foreach (IParameterSymbol p in item.MethodSymbol.Parameters)
                        {
                            Utils.CollectTypeAndMembers(p.Type, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                        }
                    }
                    if (item.Mode == Mode.Response || item.Mode == Mode.All)
                    {
                        // look at return statements in method syntax and find expression types / return expression type
                        IEnumerable<ITypeSymbol> returnTypes = Utils.FindReturnedTypes(item.MethodSyntax, compilation);
                        foreach (ITypeSymbol t in returnTypes)
                        {
                            Utils.CollectTypeAndMembers(t, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                        }
                    }

                    if (item.Mode == Mode.All)
                    {
                        // quét tất cả object new trong body
                        SemanticModel semanticModel = compilation.GetSemanticModel(item.MethodSyntax.SyntaxTree);

                        IEnumerable<ITypeSymbol> createdTypes = Utils.CollectCreatedTypes(item.MethodSyntax, semanticModel);
                        foreach (var createdType in createdTypes)
                        {
                            var named = Utils.UnwrapCollection(createdType) ?? createdType as INamedTypeSymbol;
                            if (named is null)
                                continue;

                            if (named is not null && !typesToGenerate.ContainsKey(named))
                                typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>(StringComparer.Ordinal);

                            // collect properties for it:
                            Utils.CollectTypeAndMembers(createdType, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                        }

                        IEnumerable<ITypeSymbol> declaredTypes = Utils.CollectDeclaredTypes(item.MethodSyntax, semanticModel);
                        foreach (var declaredType in declaredTypes)
                        {
                            var named = Utils.UnwrapCollection(declaredType) ?? declaredType as INamedTypeSymbol;
                            if (named is null)
                                continue;

                            if (named is not null && !typesToGenerate.ContainsKey(named))
                                typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>(StringComparer.Ordinal);

                            // collect properties for it:
                            Utils.CollectTypeAndMembers(declaredType, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                        }

                        IEnumerable<ITypeSymbol> declaredTypesWithGenerics = Utils.CollectDeclaredTypesWithGenerics(item.MethodSyntax, semanticModel);
                        foreach (var declaredTypeWithGenerics in declaredTypesWithGenerics)
                        {
                            var named = Utils.UnwrapCollection(declaredTypeWithGenerics) ?? declaredTypeWithGenerics as INamedTypeSymbol;
                            if (named is null)
                                continue;

                            if (!typesToGenerate.ContainsKey(named))
                                typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>();

                            Utils.CollectTypeAndMembers(declaredTypeWithGenerics, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                        }
                    }


                    // remove types that should be ignored (Ex: System..., Microsoft...)
                    typesToGenerate = typesToGenerate.Where(kv => !IgnoreType._skipPrefixes.Any(prefix => kv.Key.ToString().StartsWith(prefix, StringComparison.OrdinalIgnoreCase)))
                                                     .ToDictionary(kv => kv.Key, kv => kv.Value);

                    // merge typesToGenerate into aggregated
                    foreach (var kv in typesToGenerate)
                    {
                        var tSym = kv.Key;
                        if (!aggregated.TryGetValue(tSym, out var propMap))
                        {
                            propMap = new Dictionary<string, ITypeSymbol?>(StringComparer.Ordinal);
                            aggregated[tSym] = propMap;
                        }

                        // merge properties: prefer existing non-null type; otherwise take new
                        foreach (var p in kv.Value)
                        {
                            if (!propMap.TryGetValue(p.Key, out var existing))
                            {
                                propMap[p.Key] = p.Value;
                            }
                            else if (existing is null && p.Value is not null)
                            {
                                // upgrade null -> concrete type when available
                                propMap[p.Key] = p.Value;
                            }
                        }

                        if (mapNamespaces.TryGetValue(tSym, out var nsForThis))
                            foreach (var ns in nsForThis)
                                mapNamespaces[tSym].Add(ns);
                    }

                    // Add required properties from config to each type
                    Dictionary<string, string> rpConfig = config.RequiredProperties ?? new Dictionary<string, string>();
                    foreach (KeyValuePair<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> kv in aggregated)
                    {
                        foreach (KeyValuePair<string, string> rp in rpConfig)
                        {
                            if (!kv.Value.ContainsKey(rp.Key))
                            {
                                // not yet → add with type from config (not resolved ITypeSymbol yet)
                                // type of prop determined later (try to map rp.Value)
                                kv.Value[rp.Key] = null;
                            }
                        }
                    }

                    // Check log
                    //string contentLog1 = string.Join();
                    //Utils.WriteLog(spc, contentLog1);

                    // Generate source files once per aggregated type for missing types
                    // (only if type symbol not found in compilation)
                    foreach (KeyValuePair<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> kv in aggregated)
                    {
                        INamedTypeSymbol typeSymbol = kv.Key;
                        // if the type exists (has a declaration in the compilation) skip
                        if (Utils.TypeDefinedInCompilation(typeSymbol, compilation))
                            continue;

                        // Create propList with correct type
                        // mapNamespaces is Dictionary<INamedTypeSymbol, HashSet<string>> that you populated earlier
                        bool hasNamespace = mapNamespaces.TryGetValue(typeSymbol, out var nsSet); // typeSymbol = kv.Key's class
                        IEnumerable<(string PropName, string PropType)> propList = kv.Value.Select(kvp =>
                        {
                            var sym = kvp.Value; // ITypeSymbol? (detected)
                            string propTypeStr;

                            // If the symbol already exists (e.g. isTeacher -> bool),
                            // use GetFriendlyTypeName(sym)
                            if (sym is not null)
                            {
                                propTypeStr = Utils.GetTypeDisplayString(sym, nsSet);
                            }
                            // If no symbol, fallback to config.RequiredProperties(string)
                            else if (rpConfig.TryGetValue(kvp.Key, out var typeFromConfig) && !string.IsNullOrWhiteSpace(typeFromConfig))
                            {
                                propTypeStr = typeFromConfig;
                            }
                            else
                            {
                                propTypeStr = "object"; // default type
                            }
                            return (PropName: kvp.Key, PropType: propTypeStr);
                        });

                        string nameSpace = config.Namespace ?? typeSymbol.ContainingNamespace.ToDisplayString();
                        if (!mapClassesToGenerate.ContainsKey((nameSpace, typeSymbol.Name)))
                        {
                            mapClassesToGenerate[(nameSpace, typeSymbol.Name)]
                                = (typeSymbol, propList.ToList(), mapCtor, mapNamespaces);
                        }
                        else
                        {
                            List<(string propName, string propType)> currentpropList
                              = mapClassesToGenerate[(nameSpace, typeSymbol.Name)].propList;

                            Dictionary<INamedTypeSymbol, List<(string CtorKey, List<(string Name, ITypeSymbol Type)> Params)>> currentMapCtor
                              = mapClassesToGenerate[(nameSpace, typeSymbol.Name)].mapCtor;

                            var currentMapNamespaces = mapClassesToGenerate[(nameSpace, typeSymbol.Name)].mapNamespaces;

                            // merge propList, mapCtor, mapNamespaces
                            currentpropList = currentpropList.Union(propList).ToList();

                            if (mapCtor.ContainsKey(typeSymbol))
                            {
                                if (currentMapCtor.TryGetValue(typeSymbol, out var listCtor))
                                {
                                    currentMapCtor[typeSymbol] = listCtor.Concat(mapCtor[typeSymbol])
                                                                         .GroupBy(x => x.CtorKey)
                                                                         .Select(x => x.First())
                                                                         .ToList();
                                }
                                else
                                {
                                    currentMapCtor[typeSymbol] = mapCtor[typeSymbol];
                                }
                            }

                            if (hasNamespace)
                            {
                                if (currentMapNamespaces.TryGetValue(typeSymbol, out var hashSetNs))
                                {
                                    currentMapNamespaces[typeSymbol] = new HashSet<string>(currentMapNamespaces[typeSymbol].Union(nsSet));
                                }
                                else
                                {
                                    currentMapNamespaces[typeSymbol] = new HashSet<string>(nsSet.Select(x => x));
                                }
                            }

                            mapClassesToGenerate[(nameSpace, typeSymbol.Name)]
                                = (typeSymbol, currentpropList, currentMapCtor, currentMapNamespaces);
                        }
                    }
                }

                // Generate files
                if (mapClassesToGenerate.Count > 0)
                {
                    foreach (var type in mapClassesToGenerate)
                    {
                        var typeSymbol = type.Value.typeSymbol;
                        var propList = type.Value.propList;
                        var mapCtor = type.Value.mapCtor;
                        // Add an empty constructor with 0 parameters to mapCtor
                        if (mapCtor.ContainsKey(typeSymbol))
                        {
                            mapCtor[typeSymbol] = mapCtor[typeSymbol].Prepend((CtorKey: "", Params: new List<(string Name, ITypeSymbol Type)>() { }))
                                                                     .ToList();
                        }
                        var mapNamespaces = type.Value.mapNamespaces;

                        string generated = Utils.GenerateClassSource(typeSymbol, propList, config, mapCtor, mapNamespaces, spc);

                        // finally add source
                        spc.AddSource(string.Format(GeneratedFileName, typeSymbol.Name), generated);
                    }
                }
            });
        }
    }
}
