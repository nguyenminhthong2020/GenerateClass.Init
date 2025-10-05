namespace MyApp.ModelsDtos
{
	public partial class LocalFunction1z
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
		/// Prop1
		/// </summary>
		public int Prop1 { get; set; }

		/// <summary>
		/// Prop2
		/// </summary>
		public LocalFunction1zClass Prop2 { get; set; }

		/// <summary>
		/// Prop55
		/// </summary>
		public string Prop55 { get; set; }

		/// <summary>
		/// Prop66
		/// </summary>
		public LocalFunction1zClass Prop66 { get; set; }

		public LocalFunction1z()
		{
		}

		public LocalFunction1z(int Prop1, LocalFunction1zClass Prop2)
		{
		}

		public LocalFunction1z(string Prop55, LocalFunction1zClass Prop66)
		{
		}
	}
}
