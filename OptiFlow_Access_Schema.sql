
-- OptiFlow Access Database Schema (2002-2003 Format)

CREATE TABLE tblCompanies (
    CompanyID AUTOINCREMENT PRIMARY KEY,
    CompanyName TEXT(100),
    Email TEXT(100),
    CreatedDate DATETIME
);

CREATE TABLE tblUsers (
    UserID AUTOINCREMENT PRIMARY KEY,
    Username TEXT(50),
    Password TEXT(50),
    Role TEXT(20),
    CompanyID LONG
);

CREATE TABLE tblTasks (
    TaskID AUTOINCREMENT PRIMARY KEY,
    TaskName TEXT(100),
    Duration INTEGER,
    Priority TEXT(10),
    BayType TEXT(50),
    Status TEXT(20),
    StartTime DATETIME,
    CompanyID LONG
);

CREATE TABLE tblBays (
    BayID AUTOINCREMENT PRIMARY KEY,
    BayName TEXT(50),
    BayType TEXT(50),
    Status TEXT(20),
    CompanyID LONG
);

CREATE TABLE tblSchedule (
    ScheduleID AUTOINCREMENT PRIMARY KEY,
    TaskID LONG,
    BayID LONG,
    StartTime DATETIME,
    CompanyID LONG
);
