select s.StudentId, s.StudentName, g.GroupName
from Students s,
     Groups g
where s.GroupId = g.GroupId
  and s.GroupId in (
    select p.GroupId
    from Plan p,
         Courses c
    where p.CourseId = c.CourseId and c.CourseName = :CourseName
)
  and s.StudentId not in (
    select m.StudentId
    from Marks m
    where m.CourseId in (
        select c.CourseId
        from Courses c
        where c.CourseName = :CourseName
    )
);