using System.Collections.Concurrent;
using TestWebAPI.Enum;

namespace MyApp.ModelsDtos
{
	public partial class MyTestEnumList
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
		public TestEnum Prop1 { get; set; }

		/// <summary>
		/// Prop2
		/// </summary>
		public string Prop2 { get; set; }

		/// <summary>
		/// Prop0
		/// </summary>
		public (int, string, string, bool) Prop0 { get; set; }

		/// <summary>
		/// Prop00
		/// </summary>
		public (int, char) Prop00 { get; set; }

		/// <summary>
		/// PropZ
		/// </summary>
		public (TestEnum, double, int, Dictionary<string, (string, List<(int, char)>)>) PropZ { get; set; }

		/// <summary>
		/// PropZZ
		/// </summary>
		public (TestEnum, double, object, Dictionary<string, (string, List<(int, char)>)>) PropZZ { get; set; }

		/// <summary>
		/// PropZZZ
		/// </summary>
		public object PropZZZ { get; set; }

		/// <summary>
		/// PropZZZZ
		/// </summary>
		public object PropZZZZ { get; set; }

		/// <summary>
		/// PropFinalFlash
		/// </summary>
		public (int, string, object) PropFinalFlash { get; set; }

		/// <summary>
		/// PropKamehameha
		/// </summary>
		public (int, string, int, bool) PropKamehameha { get; set; }

		/// <summary>
		/// PropMakankosapo
		/// </summary>
		public (int, string, object, (int, char)) PropMakankosapo { get; set; }

		/// <summary>
		/// Galickgun
		/// </summary>
		public (int, string, object, (int, char, object)) Galickgun { get; set; }

		/// <summary>
		/// Prop3
		/// </summary>
		public List<int> Prop3 { get; set; }

		/// <summary>
		/// Prop4
		/// </summary>
		public bool Prop4 { get; set; }

		/// <summary>
		/// Prop5
		/// </summary>
		public ConcurrentDictionary<int, int> Prop5 { get; set; }

		/// <summary>
		/// Prop6
		/// </summary>
		public List<char> Prop6 { get; set; }

		/// <summary>
		/// Prop7
		/// </summary>
		public uint Prop7 { get; set; }

		/// <summary>
		/// Prop8
		/// </summary>
		public long Prop8 { get; set; }

		public MyTestEnumList()
		{
		}

		public MyTestEnumList(TestEnum Prop1, (TestEnum, double, int, Dictionary<string, (string, List<(int, char)>)>) PropZ)
		{
		}

		public MyTestEnumList(bool Prop4, ConcurrentDictionary<int, int> Prop5)
		{
		}

		public MyTestEnumList(bool Prop4, List<char> Prop6, uint Prop7, long Prop8)
		{
		}
	}
}
