using System;
using System.Collections.Generic;

namespace Microsoft.Extensions.DependencyInjection
{
    public class GenerateClassOption
    {
        public string ConfigId { get; set; }
        public string? Path { get; set; } = "./Models/";
        public bool? Enabled { get; set; } = true;
        public string? Namespace { get; set; } = null;
        public Dictionary<string, object>? RequiredProperties { get; set; } = new Dictionary<string, object>();
    }

    public static class GenerateClassServiceCollectionExtensions
    {
        public static IServiceCollection AddGenerateClass(
        this IServiceCollection services, 
        IEnumerable<GenerateClassOption> options)
        {
            foreach (var opt in options)
            {
                services.AddSingleton(opt);
            }
            return services;
        }

        public static IServiceCollection AddGenerateClass(
        this IServiceCollection services, 
        Func<IServiceProvider, IEnumerable<GenerateClassOption>> factory)
        {
            var options = factory(services.BuildServiceProvider());
            foreach (var opt in options)
            {
                services.AddSingleton(opt);
            }
            return services;
        }
    }
}
