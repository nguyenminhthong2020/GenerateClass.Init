namespace MyApp.GeneratedDtos
{
	public class MyObjM
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
		/// Age
		/// </summary>
		public int Age { get; set; }

		/// <summary>
		/// Birthday
		/// </summary>
		public string Birthday { get; set; }

		public MyObjM()
		{
		}

		public MyObjM(int Age, string Birthday)
		{
			this.Age = Age;
			this.Birthday = Birthday;
		}
	}
}
