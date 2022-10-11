drop database if exists hw4;

create database hw4;

use hw4;

create table `Lecturers`
(
    `LecturerId`   int         not null,
    `LecturerName` varchar(64) not null,
    constraint `pk_Lecturers` primary key (`LecturerId`)
);

create table `Courses`
(
    `CourseId`   int         not null,
    `CourseName` varchar(64) not null,
    constraint `pk_Courses` primary key (`CourseId`)
);

create table `Groups`
(
    `GroupId`   int      not null,
    `GroupName` char(6)  not null,
    constraint `pk_Groups` primary key (`GroupId`),
    constraint `uc_Groups_GroupName` unique (`GroupName`)
);

create table `Students`
(
    `StudentId`   int          not null,
    `StudentName` varchar(64)  not null,
    `GroupId`     int          not null,
    constraint `pk_Students` primary key (`StudentId`),
    constraint `fk_Students_GroupId` foreign key (`GroupId`)
        references `Groups` (`GroupId`)
);

create table `Plans`
(
    `GroupId`    int not null,
    `CourseId`   int not null,
    `LecturerId` int not null,
    constraint `pk_Plans` primary key (`GroupId`, `CourseId`),
    constraint `fk_Plans_GroupId` foreign key (`GroupId`)
        references `Groups` (`GroupId`),
    constraint `fk_Plans_CourseId` foreign key (`CourseId`)
        references `Courses` (`CourseId`),
    constraint `fk_Plans_LecturerId` foreign key (`LecturerId`)
        references `Lecturers` (`LecturerId`)
);

create table `Marks`
(
    `CourseId`  int not null,
    `StudentId` int not null,
    `Mark`      int not null,
    constraint `pk_Marks` primary key (
                                       `CourseId`,
                                       `StudentId`
        ),
    constraint `fk_Marks_CourseId` foreign key (`CourseId`)
        references `Courses` (`CourseId`),
    constraint `fk_Marks_StudentId` foreign key (`StudentId`)
        references `Students` (`StudentId`)
);

insert into `Courses` (
    `CourseId`,
    `CourseName`
)
values (1, 'Java Advanced'),
       (2, 'C++');

insert into `Groups` (
    `GroupId`,
    `GroupName`
)
values (1, 'M34341'),
       (2, 'M34351');

insert into `Students` (
    `StudentId`,
    `StudentName`,
    `GroupId`
)
values (1, 'Скроба Дмитрий', 1),
       (2, 'Щелочков Александр', 1);

insert into `Lecturers` (
    `LecturerId`,
    `LecturerName`
)
values (1, 'Корнеев Георгий'),
       (2, 'Скаков Павел');

insert into `Marks` (
    `CourseId`,
    `StudentId`,
    `Mark`
)
values (1, 1, 100),
       (1, 2, 60),
       (2, 1, 60),
       (2, 2, 100);

insert into `Plans` (
    `GroupId`,
    `CourseId`,
    `LecturerId`
)
values (1, 1, 1),
       (1, 2, 2),
       (2, 1, 1),
       (2, 2, 2);