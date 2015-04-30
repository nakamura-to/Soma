IF  EXISTS (SELECT name FROM sys.databases WHERE name = N'Soma.Core.IT')
	DROP DATABASE [Soma.Core.IT];
GO

create database [Soma.Core.IT];
GO
use [Soma.Core.IT];
GO
create table Department (
	DepartmentId int primary key,
	DepartmentName varchar(50) unique,
	VersionNo int not null
);
create table Employee (
	EmployeeId int identity primary key,
	EmployeeName varchar(50),
	DepartmentId int,
	VersionNo int not null
);
create table Address (
	AddressId int identity primary key,
	Street varchar(50),
	VersionNo timestamp
);
create table CompKeyEmployee (
	EmployeeId1 int not null, 
	EmployeeId2 int not null, 
	EmployeeName varchar(50),
	VersionNo int not null,
	constraint PkCompKeyEmployee primary key(EmployeeId1, EmployeeId2)
);
create table NoId (
	Name varchar(50),
	VersionNo int 
);
create table NoVersion (
	Id int primary key,
	Name varchar(50)
);
create table Person (
	PersonId int identity primary key,
	PersonName varchar(50),
	JobKind int,
	VersionNo int not null
);
create unique index IX_PersonName on Person (PersonName);
create table DefaultMapping (
	DefaultMappingId int primary key,
	ByteCol tinyint, -- Byte
	Int16Col smallint, -- Int16
	Int32Col int, -- Int32
	Int64Col bigint, -- Int64
	BinaryCol binary(10), -- Byte[]
	ImageBinaryCol image, -- Byte[]
	VarbinaryBinaryCol varbinary(10), -- Byte[]
	RowversionBinaryCol rowversion, -- Byte[]	
	BooleanCol bit, -- Boolean
	DateTimeCol datetime, -- DateTime
	DateDateTimeCol date, -- DateTime
	DateTime2Col datetime2, -- DateTime
	SmallDateTimeCol smalldatetime, -- DateTime
	DateTimeOffsetCol datetimeoffset, -- DateTimeOffset
	DecimalCol decimal(10,5), -- Decimal
	NumericDecimalCol decimal(10,5), -- Decimal
	MoneyDecimalCol money, -- Decimal
	SmallMoneyDecimalCol smallmoney, -- Decimal
	DoubleCol float, -- Double
	SingleCol real, -- Single
	VarcharStringCol varchar(20), -- String
	NVarcharStringCol nvarchar(20), -- String
	NTextStringCol ntext, -- String
	GuidCol uniqueidentifier, -- Guid
);
create table TimeSpanMapping (
	TimeSpanMappingId int primary key,
	TimeSpanCol time
);
create table LobMapping (
	LobMappingId int primary key,
	VarBinaryCol varbinary(max),
	VarcharStringCol varchar(max),
	NVarcharStringCol nvarchar(max)
);
create table CharMapping (
	CharMappingId int primary key,
	CharCol char(10)
);
GO
insert into Department values (1, 'Account', 0);
insert into Department values (2, 'Sales', 0);
set identity_insert Employee on;
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (1, 'King', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (2, 'Smith', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (3, 'Jhon', 2, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (4, null, null, 0);
set identity_insert Employee off;
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (1, 11, 'King', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (2, 12, 'Smith', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (3, 13, 'Jhon', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (4, 14, null, 0);
insert into NoId (Name, VersionNo) values ('hoge', 0);
insert into NoVersion (Id, Name) values (1, 'hoge');
set identity_insert Person on;
insert into Person (PersonId, PersonName, JobKind, VersionNo) values (1, 'Scott', 0, 0);
insert into Person (PersonId, PersonName, JobKind, VersionNo) values (2, 'Martin', 1, 0);
set identity_insert Person off;

GO
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[ProcNoneParam] 
AS
BEGIN
    SET NOCOUNT ON;
END
GO

CREATE PROCEDURE [dbo].[ProcSingleParam]
    @Param1 int
AS
BEGIN
    SET NOCOUNT ON;
END
GO

CREATE PROCEDURE [dbo].[ProcMultiParams]
    @Param1 int,
    @Param2 int OUTPUT,
    @Param3 int OUTPUT
AS
BEGIN
    SET @Param2 = @Param2 + @Param1;
    SET @Param3 = @Param1;
END
GO

CREATE PROCEDURE [dbo].[ProcResult]
    @EmployeeId int
AS
BEGIN
    SELECT EmployeeId, EmployeeName, DepartmentId, VersionNo FROM Employee WHERE EmployeeId > @EmployeeId ORDER BY EmployeeId;
END
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

CREATE PROCEDURE [dbo].[ProcResultAndUpdate]
    @EmployeeId int
AS
BEGIN
    SELECT * FROM Employee WHERE EmployeeId > @EmployeeId ORDER BY EmployeeId;
    UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
END
GO

CREATE PROCEDURE [dbo].[ProcUpdateAndResult]
    @EmployeeId int
AS
BEGIN
    UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
    SELECT * FROM Employee WHERE EmployeeId > @EmployeeId ORDER BY EmployeeId;
END
GO

CREATE PROCEDURE [dbo].[ProcResults]
    @EmployeeId int,
    @DepartmentId int
AS
BEGIN
    SELECT * FROM Employee WHERE EmployeeId > @EmployeeId ORDER BY EmployeeId;
    SELECT * FROM Department WHERE DepartmentId > @DepartmentId ORDER BY DepartmentId;
END
GO

CREATE FUNCTION [dbo].[FuncMultiParams](
    @Param1 int,
    @Param2 int)
RETURNS int
AS
BEGIN
    RETURN @Param1 + @Param2;
END
GO