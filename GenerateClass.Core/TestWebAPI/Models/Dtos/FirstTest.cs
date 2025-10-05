using TestWebAPI.Enum;

namespace MyApp.ModelsDtos
{
	public partial class FirstTest
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
		/// TestObj10
		/// </summary>
		public int[] TestObj10 { get; set; }

		/// <summary>
		/// TestObj11
		/// </summary>
		public string TestObj11 { get; set; }

		/// <summary>
		/// Area
		/// </summary>
		public Area Area { get; set; }

		/// <summary>
		/// City
		/// </summary>
		public City City { get; set; }

		/// <summary>
		/// TestObj1
		/// </summary>
		public object TestObj1 { get; set; }

		/// <summary>
		/// TestObj2
		/// </summary>
		public int? TestObj2 { get; set; }

		/// <summary>
		/// TestObj3
		/// </summary>
		public object TestObj3 { get; set; }

		/// <summary>
		/// TestObj4
		/// </summary>
		public object TestObj4 { get; set; }

		/// <summary>
		/// TestObj5
		/// </summary>
		public string TestObj5 { get; set; }

		/// <summary>
		/// TestObj6
		/// </summary>
		public DateTime? TestObj6 { get; set; }

		/// <summary>
		/// TestObj7
		/// </summary>
		public TestWebAPI.Controllers.WeatherForecastController.MyEnum TestObj7 { get; set; }

		/// <summary>
		/// TestObj8
		/// </summary>
		public TestWebAPI.Controllers.WeatherForecastController.MyStruct TestObj8 { get; set; }

		/// <summary>
		/// TestObj9
		/// </summary>
		public TestEnum TestObj9 { get; set; }

	}
}
