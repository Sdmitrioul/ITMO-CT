select s.StudentName, c.CourseName
from Students s,
     Courses c
where s.GroupId in (
    select p.GroupId
    from Plan p
    where p.CourseId = c.CourseId
)
  and s.StudentId not in (
    select m.StudentId
    from Marks m
    where m.CourseId = c.CourseId and m.Mark > 2
);