namespace MyApp.ModelsDtos
{
	public partial class Area
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
		/// AreProperty1
		/// </summary>
		public float AreProperty1 { get; set; }

		/// <summary>
		/// AreaProperty2
		/// </summary>
		public DateOnly AreaProperty2 { get; set; }

		/// <summary>
		/// AreaProperty3
		/// </summary>
		public List<List<string>> AreaProperty3 { get; set; }

		/// <summary>
		/// AreaProperty4
		/// </summary>
		public object AreaProperty4 { get; set; }

		/// <summary>
		/// AreaProperty5
		/// </summary>
		public object AreaProperty5 { get; set; }

		/// <summary>
		/// AreaProperty6
		/// </summary>
		public object AreaProperty6 { get; set; }

	}
}
