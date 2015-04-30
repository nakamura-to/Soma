#### 1.8.0.0 - June 23 2012
* New Feature - Soma.Core.CharString is added. Soma.Core.CharString maps a string with System.Data.DbType.StringFixedLength.
* New Feature - Built-in function `charString` and `charStringList` are added in Expression Language.
* Change - N' prefixed string literals are supported as test data in SQL template.
* Change - A cause exception is included in UniqueConstraintException.
* Fix - The issue about bind names in Oracle stored procedure is fixed.

####  1.7.0.1 - March 20 2012
* New Feature - Supported Oracle UDT types.
* Change - Resolved embedded variables before pagination rewriting, if only they are inside the order by clause.

####  1.6.0.1 - January 1 2012
* Change - Supported duplicated column name in SELECT clause.