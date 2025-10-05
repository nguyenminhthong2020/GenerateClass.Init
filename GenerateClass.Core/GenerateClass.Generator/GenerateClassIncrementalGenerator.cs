using GenerateClass.Generator.Core;
using GenerateClass.Generator.Enums;
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
            // For debugging
            // if (!System.Diagnostics.Debugger.IsAttached)
            //    System.Diagnostics.Debugger.Launch();

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

                        if (attributeData is null)
                            return null;

                        int mode = attributeData.ConstructorArguments.Length > 1 ?
                                  (int)(Mode)attributeData.ConstructorArguments[1].Value!
                                  : (int)(Mode.All);
                        string configId = attributeData.ConstructorArguments[0].Value!.ToString();

                        return new
                        {
                            MethodSymbol = methodSymbol,
                            MethodSyntax = methodSyntax,
                            ConfigId = configId,
                            Mode = (Mode)mode
                        };
                    })
                .Where(x => x is not null);

            // 2) Find AddGenerateClass(...) invocations to extract config (we only support simple literal initializers)
            // 2) Find AddGenerateClass(...) invocations to extract config
            var addGenConfig =
                context.SyntaxProvider.CreateSyntaxProvider<List<GenerateClassOption>>(
                    predicate: (node, ct) =>
                    {
                        if (node is InvocationExpressionSyntax inv)
                        {
                            string name = inv.Expression.ToString();
                            return name.Contains(AddGenerateClassMethod);
                        }
                        return false;
                    },
                    transform: (ctx, ct) =>
                    {
                        var inv = (InvocationExpressionSyntax)ctx.Node;
                        var arg = inv.ArgumentList.Arguments.FirstOrDefault();
                        var optsList = new List<GenerateClassOption>();
                        if (arg?.Expression is not SimpleLambdaExpressionSyntax lambda)
                            return optsList;

                        if (lambda.Body is BlockSyntax block)
                        {
                            var opt = new GenerateClassOption();
                            foreach (var stmt in block.Statements)
                            {
                                if (stmt is ExpressionStatementSyntax es && es.Expression is AssignmentExpressionSyntax assign)
                                {
                                    // left should be MemberAccessExpression e.g. config.Path
                                    if (assign.Left is MemberAccessExpressionSyntax left)
                                    {
                                        string name = left.Name.Identifier.Text;
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
                                            GenFunctions.ParseRequiredProperties(assign.Right, ctx.SemanticModel, opt.RequiredProperties);
                                        }
                                    }
                                }
                            }
                            optsList.Add(opt);
                        }
                        else if (lambda.Body is ImplicitArrayCreationExpressionSyntax implicitArray)
                        {
                            foreach (var expr in implicitArray.Initializer.Expressions)
                            {
                                if (expr is ObjectCreationExpressionSyntax obj)
                                {
                                    var opt = GenFunctions.ParseGenerateClassOptionObject(obj, ctx.SemanticModel);
                                    optsList.Add(opt);
                                }
                            }
                        }
                        else if (lambda.Body is ArrayCreationExpressionSyntax array)
                        {
                            foreach (var expr in array.Initializer!.Expressions)
                            {
                                if (expr is ObjectCreationExpressionSyntax obj)
                                {
                                    var opt = GenFunctions.ParseGenerateClassOptionObject(obj, ctx.SemanticModel);
                                    optsList.Add(opt);
                                }
                            }
                        }

                        return optsList;
                    })
                .Where(x => x is not null)
                .SelectMany((x, _) => x!)
                .Collect();

            // 3) Get the current Compilation from the generator context
            var compilationProvider = context.CompilationProvider;

            // 4) Retrieve the project directory(or a custom folder) from AnalyzerConfigOptionsProvider.
            // This lets consumers override the output folder by defining a MSBuild property in their project file.
            var projectDir = context.AnalyzerConfigOptionsProvider
                .Select((options, ct) =>
                {
                    if (options.GlobalOptions.TryGetValue("build_property.ProjectDir", out var projDir) && !string.IsNullOrWhiteSpace(projDir))
                        return projDir;

                    return null;
                });

            ///var combined = context.CompilationProvider.Combine(methodsWithAttr.Collect()).Combine(addGenConfig);
            // 5) Combine all inputs (compilation + projectDir + discovered methods + configs)
            //    and build a pipeline that will trigger code generation.
            //    A "stamp" (GUID) is added to break incremental cache and force regeneration on each build.
            var combined = compilationProvider
                    // Inject a fake stamp to force re-execution every build.
                    // Another option is to use DateTime.UtcNow.Ticks.ToString()
                    .Select((compilation, ct) => (compilation, stamp: Guid.NewGuid().ToString()))
                    .Combine(projectDir)
                    .Combine(methodsWithAttr.Collect())
                    .Combine(addGenConfig)
                    .Select((x, ct) =>
                    {
                        // x is ((((Compilation compilation, string stamp), string? projDir),
                        //         ImmutableArray<MethodInfo> methods),
                        //         ImmutableArray<GenerateClassOption?> configs)
                        var (((compilationAndStamp, projDir), methodsCollected), configsCollected) = x;

                        // Return a single tuple that contains all values needed in RegisterSourceOutput.
                        return (compilationAndStamp.compilation,
                                compilationAndStamp.stamp,
                                projDir,
                                methodsCollected,
                                configsCollected);
                    });

            context.RegisterSourceOutput(combined, (spc, source) =>
            {
                // Test 
                spc.ReportDiagnostic(Diagnostic.Create(
                new DiagnosticDescriptor(
                    id: "SG001",
                    title: "GenerateClass.Generator",
                    messageFormat: "GenerateClass.Generator is running.",
                    category: "GenerateClass.Generator",
                    DiagnosticSeverity.Warning,
                    isEnabledByDefault: true),
                Location.None));

#pragma warning disable CS8619 // Nullability of reference types in value doesn't match target type.
                //(Compilation compilation, _, var projDir, var methods, System.Collections.Immutable.ImmutableArray<GenerateClassOption> configs) = source;
                (Compilation compilation, _, var projDir, var methods, System.Collections.Immutable.ImmutableArray<GenerateClassOption> configs) = source;
#pragma warning restore CS8619 // Nullability of reference types in value doesn't match target type.

                foreach (var config in configs.Where(c => c is not null && (c.Enabled ?? true)))
                {
                    // aggregated map across all methods: TypeSymbol -> (PropName -> ITypeSymbol?)
                    Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> aggregated
                                                  = new(SymbolEqualityComparer.Default);
                    HashSet<string> usedHintNames = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                    Dictionary<(string nspace, string namedTypeSymbol),
                              (INamedTypeSymbol typeSymbol,
                              List<(string PropName, string PropType)> propList,
                              Dictionary<INamedTypeSymbol, List<(string CtorKey,
                                       List<(string? Name, ITypeSymbol Type)> Params,
                                       bool IsNamedArgs)>> mapCtor,
                              Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces)>
                       mapClassesToGenerate = new();

                    var methodsForThisConfig = methods.Where(m => m!.ConfigId == config.ConfigId || string.IsNullOrEmpty(m.ConfigId)).ToList();
                    foreach (var item in methodsForThisConfig)
                    {
                        if (item is null)
                            continue;

                        if (item.Mode == Mode.None)
                            continue;

                        // key = typeSymbol (Teacher), value = map propName -> ITypeSymbol? 
                        Dictionary<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> typesToGenerate
                                 = new(SymbolEqualityComparer.Default);

                        // var obj = new MyObject(Prop1: val1, Prop2: val2,...)
                        // Ctor: constructor symbol → collect param names and types
                        Dictionary<INamedTypeSymbol, List<(string CtorKey,
                                       List<(string? Name, ITypeSymbol Type)> Params,
                                       bool IsNamedArgs)>> mapCtor
                                 = new(SymbolEqualityComparer.Default);

                        // Namespace imports to add to generated files
                        Dictionary<INamedTypeSymbol, HashSet<string>> mapNamespaces = new();

                        // Collect based on Mode:
                        if ((item.Mode & Mode.Parameter) != 0)
                        {
                            foreach (IParameterSymbol p in item.MethodSymbol.Parameters)
                            {
                                GenFunctions.CollectTypeAndMembers(p.Type, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                            }
                        }
                        if ((item.Mode & Mode.Response) != 0)
                        {
                            // look at return statements in method syntax and find expression types / return expression type
                            IEnumerable<ITypeSymbol> returnTypes = GenFunctions.FindReturnedTypes(item.MethodSyntax, compilation);
                            foreach (ITypeSymbol t in returnTypes)
                            {
                                GenFunctions.CollectTypeAndMembers(t, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                            }
                        }

                        if ((item.Mode & Mode.Body) != 0)
                        {
                            // scan all new objects in body
                            SemanticModel semanticModel = compilation.GetSemanticModel(item.MethodSyntax.SyntaxTree);

                            IEnumerable<ITypeSymbol> createdTypes = GenFunctions.CollectCreatedTypes(item.MethodSyntax, semanticModel);
                            foreach (var createdType in createdTypes)
                            {
                                var named = GenFunctions.UnwrapCollection(createdType) ?? createdType as INamedTypeSymbol;
                                if (named is null)
                                    continue;

                                if (named is not null && !typesToGenerate.ContainsKey(named))
                                    typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>(StringComparer.Ordinal);

                                // collect properties for it:
                                GenFunctions.CollectTypeAndMembers(createdType, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                            }

                            IEnumerable<ITypeSymbol> declaredTypes = GenFunctions.CollectDeclaredTypes(item.MethodSyntax, semanticModel);
                            foreach (var declaredType in declaredTypes)
                            {
                                var named = GenFunctions.UnwrapCollection(declaredType) ?? declaredType as INamedTypeSymbol;
                                if (named is null)
                                    continue;

                                if (named is not null && !typesToGenerate.ContainsKey(named))
                                    typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>(StringComparer.Ordinal);

                                // collect properties for it:
                                GenFunctions.CollectTypeAndMembers(declaredType, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                            }

                            IEnumerable<ITypeSymbol> declaredTypesWithGenerics = GenFunctions.CollectDeclaredTypesWithGenerics(item.MethodSyntax, semanticModel);
                            foreach (var declaredTypeWithGenerics in declaredTypesWithGenerics)
                            {
                                var named = GenFunctions.UnwrapCollection(declaredTypeWithGenerics) ?? declaredTypeWithGenerics as INamedTypeSymbol;
                                if (named is null)
                                    continue;

                                if (!typesToGenerate.ContainsKey(named))
                                    typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>();

                                GenFunctions.CollectTypeAndMembers(declaredTypeWithGenerics, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
                            }

                            IEnumerable<ITypeSymbol> localFunctionTypes = GenFunctions.CollectLocalFunctionTypes(item.MethodSyntax, semanticModel);
                            foreach (var localFunctionType in localFunctionTypes)
                            {
                                var named = GenFunctions.UnwrapCollection(localFunctionType) ?? localFunctionType as INamedTypeSymbol;
                                if (named is null)
                                    continue;

                                if (!typesToGenerate.ContainsKey(named))
                                    typesToGenerate[named] = new Dictionary<string, ITypeSymbol?>();

                                GenFunctions.CollectTypeAndMembers(localFunctionType, item.MethodSyntax, compilation, typesToGenerate, mapCtor, mapNamespaces, item.MethodSymbol);
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
                        Dictionary<string, object> rpConfig = config.RequiredProperties ?? new Dictionary<string, object>();
                        foreach (var kv in aggregated)
                        {
                            foreach (var rp in rpConfig)
                            {
                                // convert object -> string (friendly)
                                string typeString;
                                if (rp.Value is string s)
                                {
                                    typeString = s;
                                }
                                else if (rp.Value is Type t)
                                {
                                    typeString = GenFunctions.GetSimpleTypeName(t);
                                }
                                else
                                {
                                    typeString = rp.Value?.ToString() ?? "object";
                                }

                                if (!kv.Value.ContainsKey(rp.Key))
                                {
                                    kv.Value[rp.Key] = null;
                                }
                            }
                        }


                        // Generate source files once per aggregated type for missing types
                        // (only if type symbol not found in compilation)
                        foreach (KeyValuePair<INamedTypeSymbol, Dictionary<string, ITypeSymbol?>> kv in aggregated)
                        {
                            INamedTypeSymbol typeSymbol = kv.Key;

                            if (!GenFunctions.ShouldGenerateType(typeSymbol))
                                continue;

                            // if the type exists (has a declaration in the compilation) skip
                            if (GenFunctions.TypeDefinedInCompilation(typeSymbol, compilation))
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
                                    propTypeStr = GenFunctions.GetTypeDisplayString(sym, nsSet);
                                }
                                // If no symbol, fallback to config.RequiredProperties(string)
                                else if (rpConfig.TryGetValue(kvp.Key, out var typeFromConfig))
                                {
                                    if (typeFromConfig is string s && !string.IsNullOrWhiteSpace(s))
                                        propTypeStr = s;
                                    else if (typeFromConfig is Type t)
                                        propTypeStr = GenFunctions.GetSimpleTypeName(t);
                                    else if (typeFromConfig is ITypeSymbol sym2)
                                        propTypeStr = GenFunctions.GetFriendlyTypeName(sym2);
                                    else
                                        propTypeStr = "object";
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

                                Dictionary<INamedTypeSymbol,
                                      List<(string CtorKey, List<(string? Name, ITypeSymbol Type)> Params, bool IsNamedArgs)>>
                                 currentMapCtor
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
                                mapCtor[typeSymbol] = mapCtor[typeSymbol]
                                            .Prepend((CtorKey: "",
                                                      Params: new List<(string? Name, ITypeSymbol Type)>(),
                                                      IsNamedArgs: false))
                                            .ToList();
                            }
                            var mapNamespaces = type.Value.mapNamespaces;

                            string generated = GenFunctions.GenerateClassSource(typeSymbol, propList, config, mapCtor, mapNamespaces, spc);

                            // finally add source
                            // spc.AddSource(string.Format(GeneratedFileName, typeSymbol.Name), generated);

                            if (!string.IsNullOrWhiteSpace(projDir))
                            {
                                var dir = Path.Combine(projDir, config.Path);
                                Directory.CreateDirectory(dir);
                                var fullFilePath = Path.Combine(dir, string.Format(GeneratedFileName, typeSymbol.Name));
                                File.WriteAllText(fullFilePath, generated);
                            }
                        }
                    }
                }
            });
        }
    }
}
