insert into Groups (GroupId, GroupName)
values
    (0, 'M34341'),
    (1, 'M34351'),
    (2, 'M34361'),
    (3, 'M34371'),
    (4, 'M34381'),
    (5, 'M32391');

insert into Lecturers (LecturerId, LecturerName)
values
    (1, 'Korneev G.A.'),
    (2, 'Yurchenko A.O.'),
    (3, 'Kuznetsova E.M.'),
    (4, 'Kirakozoff A.K.'),
    (5, 'Jakuba N.V.'),
    (6, 'Trofimyuk G.A.'),
    (7, 'Kudryashov B.D.'),
    (8, 'Kohas K.P.');

insert into Courses (CourseId, CourseName)
values
    (1, 'DBMS'),
    (2, 'Project managing'),
    (3, 'Software design'),
    (4, 'Information Theory'),
    (6, 'Mathematical Analysis'),
    (7, 'Java Tech');

insert into Students (StudentId, StudentName, GroupId)
values
    (1, 'Skroba D.V.', 1),
    (2, 'Petrov P.P.', 1),
    (3, 'Petrov P.P.', 2),
    (4, 'Cyderov C.C.', 2),
    (5, 'Shestacovich U.U.', 3),
    (6, 'Mayers U.N.', 4),
    (7, 'Noone X.X.', 2),
    (8, 'Arya Y.Y.', 2);

insert into Marks (StudentId, CourseId, Mark)
values
    (1, 1, 5),
    (2, 1, 4),
    (3, 1, 3),
    (2, 2, 3),
    (3, 2, 4),
    (4, 2, 5),
    (7, 1, 5),
    (7, 7, 5),
    (8, 7, 5),
    (5, 7, 5),
    (6, 7, 5),
    (3, 3, 3);

insert into Plan (GroupId, CourseId, LecturerId)
values
    (0, 1, 2),
    (2, 1, 1),
    (1, 2, 3),
    (1, 3, 4),
    (2, 3, 4),
    (0, 4, 5),
    (2, 4, 6),
    (1, 4, 7),
    (2, 4, 7),
    (4, 6, 8),
    (0, 7, 1),
    (2, 7, 1),
    (3, 7, 1),
    (4, 7, 1);

insert into NewMarks (StudentId, CourseId, Mark)
values
    (1, 1, 2),
    (2, 1, 4),
    (3, 2, 3),
    (2, 2, 3),
    (3, 1, 4),
    (4, 2, 5),
    (8, 1, 5),
    (7, 7, 5),
    (6, 7, 8),
    (5, 7, 5),
    (1, 7, 4),
    (3, 3, 3);
