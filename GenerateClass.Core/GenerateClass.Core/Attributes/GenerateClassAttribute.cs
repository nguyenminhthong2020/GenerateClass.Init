using GenerateClass.Generator.Enums;
using System;

namespace GenerateClass.Generator.Attributes
{
    [AttributeUsage(AttributeTargets.Method)]
    public class GenerateClassAttribute : Attribute
    {
        /// <summary>
        /// None,
        /// Parameter,
        /// Response,
        /// All
        /// </summary>
        public Mode Mode { get; set; }
        public string ConfigId { get; set; } = string.Empty;

        public GenerateClassAttribute(string configId, Mode mode = Mode.All)
        {
            Mode = mode;
            ConfigId = configId;
        }
    }
}
