namespace MyApp.GeneratedDtos
{
	public class MyRandomObject1
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
		/// RandomObject1Property
		/// </summary>
		public char RandomObject1Property { get; set; }

		/// <summary>
		/// Age
		/// </summary>
		public int Age { get; set; }

		/// <summary>
		/// Time
		/// </summary>
		public DateTime Time { get; set; }

		/// <summary>
		/// Checked
		/// </summary>
		public bool Checked { get; set; }

		public MyRandomObject1()
		{
		}

		public MyRandomObject1(char RandomObject1Property)
		{
			this.RandomObject1Property = RandomObject1Property;
		}

		public MyRandomObject1(int Age, string Name, DateTime Time, bool Checked)
		{
			this.Age = Age;
			this.Name = Name;
			this.Time = Time;
			this.Checked = Checked;
		}
	}
}
