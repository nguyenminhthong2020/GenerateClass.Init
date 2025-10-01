namespace MyApp.GeneratedDtos
{
	public class MyObjZ
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
		/// AgeZ
		/// </summary>
		public int AgeZ { get; set; }

		/// <summary>
		/// BirthdayZ
		/// </summary>
		public string BirthdayZ { get; set; }

		/// <summary>
		/// FinalBoss
		/// </summary>
		public string FinalBoss { get; set; }

		public MyObjZ()
		{
		}

		public MyObjZ(int AgeZ, string BirthdayZ)
		{
			this.AgeZ = AgeZ;
			this.BirthdayZ = BirthdayZ;
		}
	}
}
