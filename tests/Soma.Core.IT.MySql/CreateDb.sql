use `SomaCoreIT`;

create table Department (
	DepartmentId int primary key,
	DepartmentName varchar(50) unique,
	VersionNo int not null
);
create table Employee (
	EmployeeId int auto_increment primary key,
	EmployeeName varchar(50),
	DepartmentId int,
	VersionNo int not null
);
create table Address (
	AddressId int auto_increment primary key,
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

delimiter /

CREATE PROCEDURE ProcNoneParam()
BEGIN
END
/

CREATE PROCEDURE ProcSingleParam(
  Param1 INTEGER)
BEGIN
END
/

CREATE PROCEDURE ProcMultiParams(
  IN Param1 INTEGER,
  INOUT Param2 INTEGER,
  OUT Param3 INTEGER)
BEGIN
  SET Param2 = Param2 + Param1;
  SET Param3 = Param1;
END
/

CREATE PROCEDURE ProcResult(
  IN ParamEmployeeId INTEGER)
BEGIN
  SELECT EmployeeId, EmployeeName, DepartmentId, VersionNo FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
END
/

CREATE PROCEDURE ProcResultAndOut(
  IN ParamEmployeeId INTEGER,
  OUT ParamEmployeeCount INTEGER)
BEGIN
  SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  SELECT COUNT(*) INTO ParamEmployeeCount FROM EMPLOYEE;
END
/

CREATE PROCEDURE ProcResultAndUpdate(
  IN ParamEmployeeId INTEGER)
BEGIN
  SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
END
/

CREATE PROCEDURE ProcUpdateAndResult(
  IN ParamEmployeeId INTEGER)
BEGIN
  UPDATE Department SET DepartmentName = 'HOGE' WHERE DepartmentId = 1;
  SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;
END
/

CREATE PROCEDURE ProcResults(
  IN ParamEmployeeId INTEGER,
  IN ParamDepartmentId INTEGER)
BEGIN
    SELECT * FROM Employee WHERE EmployeeId > ParamEmployeeId ORDER BY EmployeeId;  
    SELECT * FROM Department WHERE DepartmentId > ParamDepartmentId ORDER BY DepartmentId;
END
/

CREATE FUNCTION FuncMultiParams(
  Param1 INTEGER,
  Param2 INTEGER) 
RETURNS INTEGER
DETERMINISTIC
BEGIN
  RETURN Param1 + Param2;
END
/
