namespace MyApp.GeneratedDtos
{
	public class MyRandomObject2
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
		/// Time
		/// </summary>
		public DateTime Time { get; set; }

		/// <summary>
		/// Checked
		/// </summary>
		public bool Checked { get; set; }

		public MyRandomObject2()
		{
		}

		public MyRandomObject2(long Id, string Name, DateTime Time, bool Checked)
		{
			this.Id = Id;
			this.Name = Name;
			this.Time = Time;
			this.Checked = Checked;
		}
	}
}
