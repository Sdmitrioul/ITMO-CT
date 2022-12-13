select s.StudentId
from Students s
where s.StudentId in (
    select m.StudentId
    from Marks m
    where m.CourseId in (
        select p.CourseId
        from Plan p
        where p.GroupId = s.GroupId
          and p.LecturerId in (
            select l.LecturerId
            from Lecturers l
            where l.LecturerName = :LecturerName
        )
    )
);