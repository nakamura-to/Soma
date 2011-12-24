drop table Department;
drop table Employee;
drop table CompKeyEmployee;
drop table NoId;
drop table NoVersion;
drop table Person;
drop table Address;
drop table DefaultMapping;

create table Department (
	DepartmentId int primary key,
	DepartmentName nvarchar(50) unique,
	VersionNo int not null
);
create table Employee (
	EmployeeId int identity(100, 1) primary key,
	EmployeeName nvarchar(50),
	DepartmentId int,
	VersionNo int not null
);
create table CompKeyEmployee (
	EmployeeId1 int not null, 
	EmployeeId2 int not null, 
	EmployeeName nvarchar(50),
	VersionNo int not null,
	constraint PkCompKeyEmployee primary key(EmployeeId1, EmployeeId2)
);
create table NoId (
	Name nvarchar(50),
	VersionNo int 
);
create table NoVersion (
	Id int primary key,
	Name nvarchar(50)
);
create table Person (
	PersonId int identity(100, 1) primary key,
	PersonName nvarchar(50),
	JobKind int,
	VersionNo int not null
);
create table Address (
	AddressId int identity(100, 1) primary key,
	Street nvarchar(50),
	VersionNo rowversion
);
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
	DecimalCol decimal(10,5), -- Decimal
	NumericDecimalCol decimal(10,5), -- Decimal
	MoneyDecimalCol money, -- Decimal
	DoubleCol float, -- Double
	SingleCol real, -- Single
	NVarcharStringCol nvarchar(20), -- String
	NTextStringCol ntext, -- String
	GuidCol uniqueidentifier -- Guid
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