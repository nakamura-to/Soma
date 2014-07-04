create sequence Employee_SEQ AS integer start with 5 increment BY 100;

create table Department (
	DepartmentId integer not null primary key,
	DepartmentName varchar(50) not null unique,
	VersionNo integer not null
);
create table Employee (
	EmployeeId integer not null primary key,
	EmployeeName varchar(50),
	DepartmentId integer,
	VersionNo integer not null
);
create table CompKeyEmployee (
	EmployeeId1 integer not null, 
	EmployeeId2 integer not null, 
	EmployeeName varchar(50),
	VersionNo integer not null,
	constraint PkCompKeyEmployee primary key(EmployeeId1, EmployeeId2)
);
create table NoId (
	Name varchar(50),
	VersionNo integer 
);
create table NoVersion (
	Id integer not null primary key,
	Name varchar(50)
);
create table Person (
	PersonId integer not null primary key,
	PersonName varchar(50),
	JobKind integer,
	VersionNo integer not null
);
--create table DefaultMapping (
--	DefaultMappingId integer not null primary key,
--	ByteCol number(3, 0),
--	BinaryCol raw(20),
--	TimeStampCol timestamp,
--	DecimalCol number,
--	DoubleCol float,
--	SingleCol float,
--	Int16Col number(5,0),
--	Int32Col number(10,0),
--	Int64Col number(19,0),
--	Varchar2StringCol varchar2(20 byte),
--	Nvarchar2StringCol nvarchar2 (20) 
--);
--create table BooleanMapping (
--  BooleanMappingId integer not null primary key,
--  BooleanCol number(1,0)
--);
--create table TimeSpanMapping (
--  TimeSpanMappingId integer not null primary key,
--  TimeSpanCol interval day (2) to second (6)
--);
--create table DateMapping (
--  DateMappingId integer not null primary key,
--  DateCol date
--);
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
commit;

CREATE PROCEDURE ProcNoneParam
LANGUAGE SQL
DYNAMIC RESULT SETS 0
BEGIN
END
/

CREATE PROCEDURE ProcSingleParam(
  IN param1 INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 0
BEGIN
END
/

CREATE PROCEDURE ProcMultiParams(
  IN param1 INTEGER,
  INOUT param2 INTEGER,
  OUT param3 INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 0
BEGIN
  SET param2 = param2 + param1;
  SET param3 = param1;
END
/

CREATE PROCEDURE ProcResult(
  IN ParamEmployeeId INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 1
BEGIN
  DECLARE c_emp CURSOR WITH RETURN FOR
    SELECT EmployeeId, EmployeeName, DepartmentId, VersionNo FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  OPEN c_emp;
END
/

CREATE PROCEDURE ProcResultAndOut(
  IN ParamEmployeeId INTEGER,
  OUT ParamEmployeeCount INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 1
BEGIN
  DECLARE c_emp CURSOR WITH RETURN FOR
    SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  OPEN c_emp;
  SELECT COUNT(*) INTO ParamEmployeeCount FROM Employee;
END
/

CREATE PROCEDURE ProcResultAndUpdate(
  IN ParamEmployeeId INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 1
BEGIN
  DECLARE c_emp CURSOR WITH RETURN FOR
    SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  OPEN c_emp;
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
END
/

CREATE PROCEDURE ProcUpdateAndResult(
  IN ParamEmployeeId INTEGER)
LANGUAGE SQL
DYNAMIC RESULT SETS 1
BEGIN
  DECLARE c_emp CURSOR WITH RETURN FOR
    SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
  OPEN c_emp;
END
/

CREATE PROCEDURE ProcResults(
  IN ParamEmployeeId INTEGER,
  IN ParamDepartmentId INTEGER)
DYNAMIC RESULT SETS 2
BEGIN
  DECLARE c_emp CURSOR WITH RETURN FOR
    SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  DECLARE c_dept CURSOR WITH RETURN FOR
    SELECT * FROM Department WHERE DepartmentId > ParamDepartmentId ORDER BY DepartmentId;
  OPEN c_emp;
  OPEN c_dept;
END
/

commit;
