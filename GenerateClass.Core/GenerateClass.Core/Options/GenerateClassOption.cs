using System;
using System.Collections.Generic;

namespace Microsoft.Extensions.DependencyInjection
{
    public class GenerateClassOption
    {
        public string? Path { get; set; } = "./Models/";
        public bool? Enabled { get; set; } = true;
        public string? Namespace { get; set; } = null;
        public Dictionary<string, string>? RequiredProperties { get; set; } = new Dictionary<string, string>();
    }

    public static class GenerateClassServiceCollectionExtensions
    {
        public static IServiceCollection AddGenerateClass(this IServiceCollection services, Action<GenerateClassOption> configure)
        {
            GenerateClassOption opts = new GenerateClassOption();
            configure(opts);
            services.AddSingleton(opts);
            return services;
        }

        public static IServiceCollection AddGenerateClass(this IServiceCollection services, GenerateClassOption opts)
        {
            services.AddSingleton(opts);
            return services;
        }
    }

}
