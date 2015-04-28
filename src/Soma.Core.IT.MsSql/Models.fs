[<AutoOpen>]
module Models
open Soma.Core

type Department =
    { [<Id>]
      DepartmentId : int
      DepartmentName : string
      [<Version>]
      VersionNo : int }

type Employee =
    { [<Id(IdKind.Identity)>]
      EmployeeId : int option
      EmployeeName : string option
      DepartmentId : int option
      [<Version>]
      VersionNo : int option }

type Address =
    { [<Id(IdKind.Identity)>]
      AddressId : int
      Street : string
      [<Version(VersionKind.Computed)>]
      VersionNo : byte array }

type JobKind =
| Salesman = 0
| Manager = 1

[<Table("Person")>]
type PersonDifferent =
    { [<Id(IdKind.Identity)>]
      [<Column("PersonId")>]PersonIdentifier : int
      [<Column("PersonName")>]PersonNameOf : string
      JobKind : JobKind
      [<Version>]
      VersionNo : int }

type Person =
    { [<Id(IdKind.Identity)>]
      PersonId : int
      PersonName : string
      JobKind : JobKind
      [<Version>]
      VersionNo : int }

type CompKeyEmployee =
    { [<Id>]
      EmployeeId1 : int
      [<Id>]
      EmployeeId2 : int
      EmployeeName : string
      [<Version>]
      VersionNo : int }

type Duplication =
    { aaa : int
      aaa1 : int
      bbb : string }
      
type NoId =
    { Name : string
      VersionNo : int }

type NoVersion =
    { [<Id>]
      Id : int
      Name : string }