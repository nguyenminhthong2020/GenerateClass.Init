namespace MyApp.GeneratedDtos
{
	public class School
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
		/// SchoolMail
		/// </summary>
		public string SchoolMail { get; set; }

		/// <summary>
		/// SchoolPhone
		/// </summary>
		public string SchoolPhone { get; set; }

		/// <summary>
		/// SchoolMail4
		/// </summary>
		public string SchoolMail4 { get; set; }

		/// <summary>
		/// SchoolPhone4
		/// </summary>
		public bool SchoolPhone4 { get; set; }

		/// <summary>
		/// SchoolTest4
		/// </summary>
		public string SchoolTest4 { get; set; }

		/// <summary>
		/// SchoolMail5
		/// </summary>
		public string SchoolMail5 { get; set; }

		/// <summary>
		/// SchoolPhone5
		/// </summary>
		public string SchoolPhone5 { get; set; }

		/// <summary>
		/// S5
		/// </summary>
		public bool S5 { get; set; }

		/// <summary>
		/// SchoolMail6
		/// </summary>
		public string SchoolMail6 { get; set; }

		/// <summary>
		/// SchoolPhone6
		/// </summary>
		public string SchoolPhone6 { get; set; }

		/// <summary>
		/// S6
		/// </summary>
		public bool S6 { get; set; }

		/// <summary>
		/// S6x
		/// </summary>
		public int S6x { get; set; }

		/// <summary>
		/// NewSchoolProperty
		/// </summary>
		public string NewSchoolProperty { get; set; }

		public School()
		{
		}

		public School(long Id, string SchoolPhone)
		{
			this.Id = Id;
			this.SchoolPhone = SchoolPhone;
		}

		public School(string SchoolMail, string SchoolPhone)
		{
			this.SchoolMail = SchoolMail;
			this.SchoolPhone = SchoolPhone;
		}

		public School(string SchoolMail4, bool SchoolPhone4, string SchoolTest4)
		{
			this.SchoolMail4 = SchoolMail4;
			this.SchoolPhone4 = SchoolPhone4;
			this.SchoolTest4 = SchoolTest4;
		}

		public School(string SchoolMail5, string SchoolPhone5, bool S5)
		{
			this.SchoolMail5 = SchoolMail5;
			this.SchoolPhone5 = SchoolPhone5;
			this.S5 = S5;
		}

		public School(string SchoolMail6, string SchoolPhone6, bool S6, int S6x)
		{
			this.SchoolMail6 = SchoolMail6;
			this.SchoolPhone6 = SchoolPhone6;
			this.S6 = S6;
			this.S6x = S6x;
		}
	}
}
