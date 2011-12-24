'----------------------------------------------------------------------------
'
' Copyright (c) 2011 The Soma Team. 
'
' This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
' copy of the license can be found in the License.txt file at the root of this distribution. 
' By using this source code in any fashion, you are agreeing to be bound 
' by the terms of the Apache License, Version 2.0.
'
' You must not remove this notice, or any other, from this software.
'----------------------------------------------------------------------------

Imports System
Imports System.Transactions
Imports Soma.Core

Module Module1

    ' define a config class
    Class MyConfig
        Inherits MsSqlConfig
        Public Overrides ReadOnly Property ConnectionString As String
            Get
                Return "Data Source=.\SQLEXPRESS;Initial Catalog=Soma.Tutorial;Integrated Security=True"
            End Get
        End Property
    End Class

    ' define a class mapped to a table
    Class Employee
        <Id(IdKind.Identity)>
        Public Property EmployeeId As Integer
        Public Property EmployeeName As String
        Public Property DepartmentId As Integer
        <Version()>
        Public Property VersionNo As Integer

        Public Overrides Function ToString() As String
            Dim fmt = "EmployeeId: {0}, EmployeeName: {1}, DepartmentId: {2}, VersionNo: {3}"
            Return String.Format(fmt, EmployeeId, EmployeeName, DepartmentId, VersionNo)
        End Function
    End Class

    ' define a class mapped to a procedure
    Class ProcResultAndOut
        Public Property EmployeeId As Integer
        <ProcedureParam(Direction:=Direction.Output)>
        Public Property EmployeeCount As Integer
        <ProcedureParam(Direction:=Direction.Result)>
        Public Property Employees As IList(Of Employee)
    End Class

    Sub Main()
        Dim db As New Db(New MyConfig)

        ' execute following code in a transaction, but don't commit
        Using New TransactionScope
            ' find by id
            Dim emp = db.Find(Of Employee)(1)
            Console.WriteLine("FOUND RECORD :" + vbCrLf + "{0}" + vbCrLf, emp)

            ' update
            emp.EmployeeName = "Hoge"
            db.Update(emp)
            Console.WriteLine("UPDATED RECORD : " + vbCrLf + "{0}" + vbCrLf, emp)

            ' delete
            db.Delete(emp)
            Console.WriteLine("DELETED RECORD : " + vbCrLf + "{0}" + vbCrLf, emp)

            ' insert
            emp = New Employee
            emp.EmployeeName = "Allen"
            emp.DepartmentId = 2
            db.Insert(emp)
            Console.WriteLine("INSERTED RECORD : " + vbCrLf + "{0}" + vbCrLf, emp)

            ' query by SQL. parameters are bindable with "Anonymous Types".
            Dim empList As List(Of Employee) =
             db.Query(Of Employee)(
              "select " +
              "	e.EmployeeId, " +
              "	e.EmployeeName, " +
              "	e.DepartmentId, " +
              "	e.VersionNo " +
              "from " +
              "	Employee e " +
              "where " +
              "	e.DepartmentId = /* emp.DepartmentId */0", New With {emp})
            Console.WriteLine("QUERRY RESULTS AS ENTITIES :" + vbCrLf)
            For Each e In empList
                Console.WriteLine(e)
            Next
            Console.WriteLine()

            ' execute procedure
            Dim procedure = New ProcResultAndOut()
            procedure.EmployeeId = 1
            db.Call(Of ProcResultAndOut)(procedure)
            Console.WriteLine("PROCEDURE OUTPUT VALUE : \n{0}\n", procedure.EmployeeCount)
            Console.WriteLine("PROCEDURE RESULT ENTITIES :")
            For Each e In procedure.Employees
                Console.WriteLine(e)
            Next
            Console.WriteLine()

            ' exequte arbitrary SQL
            Dim rows = db.Execute("delete from Employee")
            Console.WriteLine("AFFECTED ROWS : " + vbCrLf + "{0}" + vbCrLf, rows)
        End Using

        Console.ReadKey()
    End Sub

End Module
