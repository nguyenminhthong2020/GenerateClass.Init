namespace MyApp.GeneratedDtos
{
	public class MyObjN
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
		/// Age1
		/// </summary>
		public int Age1 { get; set; }

		/// <summary>
		/// Birthday1
		/// </summary>
		public string Birthday1 { get; set; }

		public MyObjN()
		{
		}

		public MyObjN(int Age1, string Birthday1)
		{
			this.Age1 = Age1;
			this.Birthday1 = Birthday1;
		}
	}
}
