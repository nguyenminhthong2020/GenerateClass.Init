using System.Collections.Concurrent;
using TestWebAPI.Enum;

namespace MyApp.GeneratedDtos
{
	public class MyTestEnumList
	{
		/// <summary>
		/// Id
		/// </summary>
		public long Id { get; set; }

		/// <summary>
		/// Name
		/// </summary>
		public string Name { get; set; }

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
			this.Prop1 = Prop1;
			this.PropZ = PropZ;
		}

		public MyTestEnumList(bool Prop4, ConcurrentDictionary<int, int> Prop5)
		{
			this.Prop4 = Prop4;
			this.Prop5 = Prop5;
		}

		public MyTestEnumList(bool Prop4, List<char> Prop6, uint Prop7, long Prop8)
		{
			this.Prop4 = Prop4;
			this.Prop6 = Prop6;
			this.Prop7 = Prop7;
			this.Prop8 = Prop8;
		}
	}
}
