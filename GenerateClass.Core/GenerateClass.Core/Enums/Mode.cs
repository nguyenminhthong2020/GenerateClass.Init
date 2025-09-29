using System;

namespace GenerateClass.Generator.Enums
{
    [Flags]
    public enum Mode
    {
        None = 0,
        Parameter = 1 << 0,
        Response = 1 << 1,
        All = Parameter | Response
    }
}
