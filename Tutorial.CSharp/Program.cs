//----------------------------------------------------------------------------
//
// Copyright (c) 2011 The Soma Team. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.txt file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Transactions;

using Soma.Core;

namespace Tutorial.CSharp
{
    // define a config class
    internal class MyConfig : MsSqlConfig
    {
        public override string ConnectionString { get { return @"Data Source=.\SQLEXPRESS;Initial Catalog=Soma.Tutorial;Integrated Security=True"; } }
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

        public override string ToString()
        {
            const string fmt = "EmployeeId: {0}, EmployeeName: {1}, DepartmentId: {2}, VersionNo: {3}";
            return string.Format(fmt, EmployeeId, EmployeeName, DepartmentId, VersionNo);
        }
    }

    // define a class mapped to a procedure
    internal class ProcResultAndOut
    {
        public int EmployeeId { get; set; }
        [ProcedureParam(Direction = Direction.Output)]
        public int EmployeeCount { get; set; }
        [ProcedureParam(Direction = Direction.Result)]
        public IList<Employee> Employees { get; set; }
    }

    internal class Program
    {
        private static void Main()
        {
            var db = new Db(new MyConfig());

            // execute following code in a transaction, but don't commit
            using (new TransactionScope())
            {
                // find by id
                var emp = db.Find<Employee>(1);
                Console.WriteLine("FOUND ENTITY : \n{0}\n", emp);

                // update
                emp.EmployeeName = "Hoge";
                db.Update(emp);
                Console.WriteLine("UPDATED ENTITY : \n{0}\n", emp);

                // delete
                db.Delete(emp);
                Console.WriteLine("DELETED ENTITY : \n{0}\n", emp);

                // insert
                emp = new Employee { EmployeeName = "Allen", DepartmentId = 2 };
                db.Insert(emp);
                Console.WriteLine("INSERTED ENTITY : \n{0}\n", emp);

                // query and map results to entities. parameters are bindable with "Anonymous Types".
                var empList = db.Query<Employee>(@"
                    select 
                        e.EmployeeId,
                        e.EmployeeName,
                        e.DepartmentId,
                        e.VersionNo
                    from
                        Employee e
                    where
                        e.DepartmentId = /* emp.DepartmentId */0
                    ", new { emp });
                Console.WriteLine("QUERRY RESULTS AS ENTITIES :");
                foreach (var e in empList)
                {
                    Console.WriteLine(e);
                }
                Console.WriteLine();

                // query and map results to dynamic objects. parameters are bindable with "Anonymous Types".
                var empList2 = db.Query<dynamic>(@"
                    select 
                        e.EmployeeId,
                        e.EmployeeName,
                        e.DepartmentId,
                        e.VersionNo
                    from
                        Employee e
                    where
                        e.DepartmentId = /* emp.DepartmentId */0
                    ", new { emp });
                Console.WriteLine("QUERY RESULTS AS DYNAMIC OBJECTS :");
                foreach (var e in empList2)
                {
                    Console.WriteLine("EmployeeId={0}, EmployeeName={1}", e.EmployeeId, e.EmployeeName);
                }
                Console.WriteLine();

                // call procedure
                var procedure = new ProcResultAndOut { EmployeeId = 1 };
                db.Call(procedure);
                Console.WriteLine("PROCEDURE OUTPUT VALUE : \n{0}\n", procedure.EmployeeCount);
                Console.WriteLine("PROCEDURE RESULT ENTITIES :");
                foreach (var e in procedure.Employees)
                {
                    Console.WriteLine(e);
                }
                Console.WriteLine();

                // execute arbitrary SQL
                var rows = db.Execute("delete from Employee");
                Console.WriteLine("AFFECTED ROWS : \n{0}\n", rows);
            }
            Console.ReadKey();
        }
    }
}