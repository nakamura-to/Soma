using System;

namespace Soma.Core.PT.CSharp
{
	// define a config class
	internal class MyConfig : MsSqlConfig
	{
		public override string ConnectionString { get { return "Data Source=.;Initial Catalog=Soma.Tutorial;Integrated Security=True"; } }
		public override Action<PreparedStatement> Logger { get { return SilentLogger; } }
	}

	// define a class mapped to a table
	internal class Employee
	{
		[Id(IdKind.Identity)]
		public int EmployeeId { get; set; }
		public string EmployeeName { get; set; }
		public int DepartmentId { get; set; }
		[Version]
		public int VersionNo { get; set; }
	}

	internal class Program
	{
		private static void Main(string[] args)
		{
			var db = new Db(new MyConfig());

			for (int i = 0; i < 500; i++)
			{
				db.Query<Employee>("select * from Employee where EmployeeId = /*id*/0", new {id = 1});
			}
			Console.WriteLine("Done");
		}
	}
}