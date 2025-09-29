using System;

namespace MyApp.GeneratedDtos
{
	public class MyRandomObject1
	{
		/// <summary>
		/// Age
		/// </summary>
		public int Age { get; set; }

		/// <summary>
		/// Name
		/// </summary>
		public string Name { get; set; }

		/// <summary>
		/// Time
		/// </summary>
		public DateTime Time { get; set; }

		/// <summary>
		/// Checked
		/// </summary>
		public bool Checked { get; set; }

		/// <summary>
		/// Id
		/// </summary>
		public long Id { get; set; }

		public MyRandomObject1(int Age, string Name, System.DateTime Time, bool Checked)
		{
			this.Age = Age;
			this.Name = Name;
			this.Time = Time;
			this.Checked = Checked;
		}
	}
}
