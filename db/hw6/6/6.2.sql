select g.GroupName, c.CourseName
from Groups g,
     Courses c
where not exists(
        select s.StudentId
        from Students s
        where s.GroupId = g.GroupId
          and s.StudentId not in (
            select m.StudentId
            from Marks m
            where m.CourseId = c.CourseId
        )
    );