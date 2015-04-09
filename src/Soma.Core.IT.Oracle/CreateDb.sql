create sequence Employee_SEQ increment by 5 start with 100;

create table Department (
	DepartmentId int primary key,
	DepartmentName varchar(50) unique,
	VersionNo int not null
);
create table Employee (
	EmployeeId int primary key,
	EmployeeName varchar(50),
	DepartmentId int,
	VersionNo int not null
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
	PersonId int primary key,
	PersonName varchar(50),
	JobKind int,
	VersionNo int not null
);
create table DefaultMapping (
	DefaultMappingId int primary key,
	ByteCol number(3, 0),
	BinaryCol raw(20),
	TimeStampCol timestamp,
	DecimalCol number,
	DoubleCol float,
	SingleCol float,
	Int16Col number(5,0),
	Int32Col number(10,0),
	Int64Col number(19,0),
	Varchar2StringCol varchar2(20 byte),
	Nvarchar2StringCol nvarchar2 (20) 
);
create table BooleanMapping (
  BooleanMappingId int primary key,
  BooleanCol number(1,0)
);
create table TimeSpanMapping (
  TimeSpanMappingId int primary key,
  TimeSpanCol interval day (2) to second (6)
);
create table DateMapping (
  DateMappingId int primary key,
  DateCol date
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
commit;

CREATE OR REPLACE PROCEDURE ProcNoneParam AS
BEGIN
  NULL;
END ProcNoneParam;
/

CREATE OR REPLACE PROCEDURE ProcSingleParam
( Param1 IN NUMBER
) AS
BEGIN
  NULL;
END ProcSingleParam;
/

CREATE OR REPLACE PROCEDURE ProcMultiParams
( Param1 IN NUMBER
, Param2 IN OUT NUMBER
, Param3 OUT NUMBER
) AS
BEGIN
  param2 := param2 + param1;
  param3 := param1;
  NULL;
END ProcMultiParams;
/

CREATE OR REPLACE PROCEDURE ProcResult 
(  ParamEmployeeId IN NUMERIC,
   cur OUT SYS_REFCURSOR
) AS
BEGIN
  OPEN cur FOR SELECT EmployeeId, EmployeeName, DepartmentId, VersionNo FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
END ProcResult;
/

CREATE OR REPLACE PROCEDURE ProcResultAndOut
( ParamEmployeeId IN NUMERIC,
  cur OUT SYS_REFCURSOR,
  ParamEmployeeCount OUT NUMERIC
) AS
BEGIN
  OPEN cur FOR SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  SELECT COUNT(*) INTO ParamEmployeeCount FROM EMPLOYEE;
END ProcResultAndOut;
/

CREATE OR REPLACE PROCEDURE ProcResultAndUpdate
( ParamEmployeeId IN NUMERIC,
  cur OUT SYS_REFCURSOR
) AS
BEGIN
  OPEN cur FOR SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
END ProcResultAndUpdate;
/

CREATE OR REPLACE PROCEDURE ProcUpdateAndResult
(  ParamEmployeeId IN NUMERIC,
   cur OUT SYS_REFCURSOR
) AS
BEGIN
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
  OPEN cur FOR SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
END ProcUpdateAndResult;
/

CREATE OR REPLACE PROCEDURE ProcResults
(  ParamEmployeeId IN NUMERIC, 
  ParamDepartmentId IN NUMERIC,
  empCur OUT SYS_REFCURSOR, 
  deptCur OUT SYS_REFCURSOR
) AS
BEGIN
  OPEN empCur FOR SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  OPEN deptCur FOR SELECT * FROM Department WHERE DepartmentId > ParamDepartmentId ORDER BY DepartmentId;
END ProcResults;
/

CREATE OR REPLACE FUNCTION FuncMultiParams
( param1 IN NUMBER, 
  param2 IN NUMBER) 
RETURN NUMBER
AS
BEGIN
  RETURN param2 + param1;
END FuncMultiParams;
/

CREATE OR REPLACE PROCEDURE ProcTimeSpanSingleParam
( Param1 IN OUT interval day to second) 
AS
BEGIN
  NULL;
END ProcTimeSpanSingleParam;
/

CREATE OR REPLACE PROCEDURE ProcDateSingleParam
( Param1 IN OUT date) 
AS
BEGIN
  NULL;
END ProcDateSingleParam;
/

commit;