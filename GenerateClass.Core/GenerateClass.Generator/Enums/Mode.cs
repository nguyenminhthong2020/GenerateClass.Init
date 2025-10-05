namespace GenerateClass.Generator.Enums
{
    [Flags]
    public enum Mode
    {
        None = 0,
        Parameter = 1 << 0,
        Response = 1 << 1,
        Body = 1 << 2,
        All = Parameter | Response | Body
    }
}
