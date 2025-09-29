using System;

namespace MyApp.GeneratedDtos
{
	public class MyRandomObject2
	{
		/// <summary>
		/// Id
		/// </summary>
		public int Id { get; set; }

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

		public MyRandomObject2(int Id, string Name, System.DateTime Time, bool Checked)
		{
			this.Id = Id;
			this.Name = Name;
			this.Time = Time;
			this.Checked = Checked;
		}
	}
}
