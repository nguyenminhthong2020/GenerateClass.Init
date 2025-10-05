namespace MyApp.ModelsDtos
{
	public partial class LocalFunction1yZZ
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
		/// PropA
		/// </summary>
		public int PropA { get; set; }

		/// <summary>
		/// PropB
		/// </summary>
		public LocalFunction1yClass PropB { get; set; }

		public LocalFunction1yZZ()
		{
		}

		public LocalFunction1yZZ(int PropA, LocalFunction1yClass PropB)
		{
		}
	}
}
