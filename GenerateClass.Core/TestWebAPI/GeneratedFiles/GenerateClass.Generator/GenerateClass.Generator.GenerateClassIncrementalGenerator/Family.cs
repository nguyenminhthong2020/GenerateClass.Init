using System;
using System.Collections.Generic;

namespace MyApp.GeneratedDtos
{
	public class Family
	{
		/// <summary>
		/// Adress
		/// </summary>
		public string Adress { get; set; }

		/// <summary>
		/// Students
		/// </summary>
		public List<Student> Students { get; set; }

		/// <summary>
		/// Classes
		/// </summary>
		public List<TeacherClass> Classes { get; set; }

		/// <summary>
		/// Id
		/// </summary>
		public long Id { get; set; }

		/// <summary>
		/// Name
		/// </summary>
		public string Name { get; set; }

	}
}
