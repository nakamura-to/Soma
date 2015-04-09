IF  EXISTS (SELECT name FROM sys.databases WHERE name = N'Soma.Tutorial')
	DROP DATABASE [Soma.Tutorial];
GO
create database [Soma.Tutorial];
GO
use [Soma.Tutorial];
GO
create table Department (
	DepartmentId int identity primary key,
	DepartmentName varchar(50) unique,
	VersionNo int not null
);
create table Employee (
	EmployeeId int identity primary key,
	EmployeeName varchar(50) unique,
	DepartmentId int not null,
	VersionNo int not null
);
GO
set identity_insert Department on;
insert into Department (DepartmentId, DepartmentName, VersionNo) values (1, 'Account', 0);
insert into Department (DepartmentId, DepartmentName, VersionNo) values (2, 'Sales', 0);
set identity_insert Department off;
set identity_insert Employee on;
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (1, 'King', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (2, 'Smith', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (3, 'Jhon', 2, 0);
set identity_insert Employee off;
GO
CREATE PROCEDURE [dbo].[ProcResultAndOut]
    @EmployeeId int,
    @EmployeeCount int OUTPUT
AS
BEGIN
    SELECT * FROM Employee WHERE EmployeeId > @EmployeeId ORDER BY EmployeeId;
    SELECT @EmployeeCount = COUNT(*) FROM Employee;
END
GO