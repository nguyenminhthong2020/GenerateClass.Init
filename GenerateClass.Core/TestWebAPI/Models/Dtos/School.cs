namespace MyApp.ModelsDtos
{
	public partial class School
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
		/// SchoolMail
		/// </summary>
		public string SchoolMail { get; set; }

		/// <summary>
		/// SchoolPhone
		/// </summary>
		public string SchoolPhone { get; set; }

		/// <summary>
		/// SchoolMail4
		/// </summary>
		public string SchoolMail4 { get; set; }

		/// <summary>
		/// SchoolPhone4
		/// </summary>
		public bool SchoolPhone4 { get; set; }

		/// <summary>
		/// SchoolTest4
		/// </summary>
		public string SchoolTest4 { get; set; }

		/// <summary>
		/// SchoolMail6
		/// </summary>
		public string SchoolMail6 { get; set; }

		/// <summary>
		/// SchoolPhone6
		/// </summary>
		public string SchoolPhone6 { get; set; }

		/// <summary>
		/// S6
		/// </summary>
		public bool S6 { get; set; }

		/// <summary>
		/// S6x
		/// </summary>
		public int S6x { get; set; }

		/// <summary>
		/// NewSchoolProperty
		/// </summary>
		public string NewSchoolProperty { get; set; }

		public School()
		{
		}

		public School(int Id, string SchoolPhone)
		{
		}

		public School(string SchoolMail, string SchoolPhone)
		{
		}

		public School(string SchoolMail4, bool SchoolPhone4, string SchoolTest4)
		{
		}

		public School(string SchoolMail6, string SchoolPhone6, bool S6, int S6x)
		{
		}
	}
}
