create table Groups (
    GroupId integer primary key not null ,
    GroupName char(6) not null unique
);

create table Students (
    StudentId integer primary key not null,
    StudentName varchar(256) not null,
    GroupId integer references Groups(GroupId)
);

create table Courses (
    CourseId integer primary key not null,
    CourseName varchar(256) not null
);

create table Lecturers (
    LecturerId integer primary key not null,
    LecturerName varchar(256) not null
);

create table Plan (
    GroupId integer references Groups (GroupId),
    CourseId integer references Courses (CourseId),
    LecturerId integer references Lecturers (LecturerId)
);

create table Marks (
    StudentId integer references Students (StudentId),
    CourseId integer references Courses (CourseId),
    Mark integer not null,
    primary key (StudentId, CourseId)
);

create table NewMarks (
    StudentId integer references Students (StudentId),
    CourseId integer references Courses (CourseId),
    Mark integer not null,
    primary key (StudentId, CourseId)
);