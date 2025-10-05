namespace TestWebAPI
{
    public record class TestRecord<T1, T2>
    {
        public List<T1> Items { get; init; } = new List<T1>();
        public T2 Item2 { get; set; }
        public string PrintArr(params int[] arr)
        {
            string s = string.Empty;
            for(int i = 0; i < arr.Length; i++)
            {
                s += arr[i];
                if(i < Items.Count)
                {
                    s += Items[i];
                }
            }
            return s;
        }
        public string PrintList(params List<int> list)
        {
            return string.Join(",", list) + Item2.ToString();
        }
    }
}
