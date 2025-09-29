using GenerateClass.Generator.Enums;

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
        public Mode Mode { get; }

        public GenerateClassAttribute(Mode mode = Mode.All)
        {
            Mode = mode;
        }
    }
}
