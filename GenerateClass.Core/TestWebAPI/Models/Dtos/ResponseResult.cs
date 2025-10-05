namespace MyApp.ModelsDtos
{
	public partial class ResponseResult
	{
		/// <summary>
		/// Id
		/// </summary>
		public long Id { get; set; }

		/// <summary>
		/// Name1
		/// </summary>
		public string Name1 { get; set; }

		/// <summary>
		/// Name2
		/// </summary>
		public string Name2 { get; set; }

		/// <summary>
		/// CollerationId1
		/// </summary>
		public int CollerationId1 { get; set; }

		/// <summary>
		/// CollerationId2
		/// </summary>
		public short CollerationId2 { get; set; }

		/// <summary>
		/// CollerationId3
		/// </summary>
		public Int64 CollerationId3 { get; set; }

		/// <summary>
		/// CustomList1
		/// </summary>
		public List<int> CustomList1 { get; set; }

		/// <summary>
		/// CustomList2
		/// </summary>
		public List<int> CustomList2 { get; set; }

		/// <summary>
		/// CustomDictionary
		/// </summary>
		public Dictionary<string, char> CustomDictionary { get; set; }

		/// <summary>
		/// NewID
		/// </summary>
		public string NewID { get; set; }

		/// <summary>
		/// isCheck
		/// </summary>
		public bool isCheck { get; set; }

		/// <summary>
		/// RandomNumber1
		/// </summary>
		public double RandomNumber1 { get; set; }

		/// <summary>
		/// RandomNumber2
		/// </summary>
		public double RandomNumber2 { get; set; }

		/// <summary>
		/// RandomNumber3
		/// </summary>
		public double RandomNumber3 { get; set; }

		/// <summary>
		/// RandomNumber4
		/// </summary>
		public long RandomNumber4 { get; set; }

		/// <summary>
		/// RandomNumber5
		/// </summary>
		public float RandomNumber5 { get; set; }

		/// <summary>
		/// Dict
		/// </summary>
		public Dictionary<char, int> Dict { get; set; }

		/// <summary>
		/// Hashset
		/// </summary>
		public HashSet<char> Hashset { get; set; }

		/// <summary>
		/// Arr
		/// </summary>
		public decimal[][] Arr { get; set; }

		/// <summary>
		/// Arr1
		/// </summary>
		public object Arr1 { get; set; }

		/// <summary>
		/// Arr2
		/// </summary>
		public byte[] Arr2 { get; set; }

		/// <summary>
		/// Arr3
		/// </summary>
		public short[] Arr3 { get; set; }

		/// <summary>
		/// myList1
		/// </summary>
		public List<List<string>> myList1 { get; set; }

		/// <summary>
		/// myList2
		/// </summary>
		public List<List<(string, int)>> myList2 { get; set; }

		/// <summary>
		/// myList3
		/// </summary>
		public List<(int, char)> myList3 { get; set; }

		/// <summary>
		/// obj
		/// </summary>
		public object obj { get; set; }

		/// <summary>
		/// Dt
		/// </summary>
		public DateTime Dt { get; set; }

	}
}
