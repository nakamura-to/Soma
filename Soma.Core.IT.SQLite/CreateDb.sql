create table Department (
	DepartmentId integer primary key,
	DepartmentName text unique,
	VersionNo integer not null
);
create table Employee (
	EmployeeId integer primary key,
	EmployeeName text,
	DepartmentId integer,
	VersionNo integer not null
);
create table CompKeyEmployee (
	EmployeeId1 integer not null, 
	EmployeeId2 integer not null, 
	EmployeeName text,
	VersionNo integer not null,
	constraint PkCompKeyEmployee primary key(EmployeeId1, EmployeeId2)
);
create table NoId (
	Name text,
	VersionNo integer 
);
create table NoVersion (
	Id integer primary key,
	Name text
);
create table Person (
	PersonId integer primary key,
	PersonName text,
	JobKind integer,
	VersionNo integer not null
);
create table DefaultMapping (
	DefaultMappingId integer primary key,
	ByteCol integer, -- Byte
	Int16Col integer, -- Int16
	Int32Col integer, -- Int32
	Int64Col integer, -- Int64
	BytesCol blob, -- Byte[]
	BooleanCol numeric, -- Boolean
	DateTimeCol text, -- DateTime
	DecimalCol numeric, -- Decimal
	DoubleCol real, -- Double
	SingleCol real, -- Single
	StringCol text, -- String
	GuidCol blob -- Guid
);
insert into Department values (1, 'Account', 0);
insert into Department values (2, 'Sales', 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (1, 'King', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (2, 'Smith', 1, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (3, 'Jhon', 2, 0);
insert into Employee (EmployeeId, EmployeeName, DepartmentId, VersionNo) values (4, null, null, 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (1, 11, 'King', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (2, 12, 'Smith', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (3, 13, 'Jhon', 0);
insert into CompKeyEmployee (EmployeeId1, EmployeeId2, EmployeeName, VersionNo) values (4, 14, null, 0);
insert into NoId (Name, VersionNo) values ('hoge', 0);
insert into NoVersion (Id, Name) values (1, 'hoge');
insert into Person (PersonId, PersonName, JobKind, VersionNo) values (1, 'Scott', 0, 0);
insert into Person (PersonId, PersonName, JobKind, VersionNo) values (2, 'Martin', 1, 0);
