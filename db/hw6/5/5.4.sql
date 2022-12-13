select s.StudentId
from Students s
where not exists(
        select p.CourseId
        from Plan p
        where p.GroupId = s.GroupId
          and p.LecturerId in (
            select l.LecturerId
            from Lecturers l
            where l.LecturerName = :LecturerName
        )
          and p.CourseId not in (
            select m.CourseId
            from Marks m
            where m.StudentId = s.StudentId
        )
    );
