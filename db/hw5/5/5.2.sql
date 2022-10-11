select StudentName, CourseName
from (
select distinct c.CourseName, s.StudentName, s.StudentId, c.CourseId
from Plan p
    inner join Courses c on c.CourseId = p.CourseId
    inner join Students s on s.GroupId = p.GroupId
except
select distinct c.CourseName, s.StudentName, s.StudentId, c.CourseId
from Students s
    inner join Marks m on m.StudentId = s.StudentId
    inner join Courses c on c.CourseId = m.CourseId
    inner join Plan p on p.CourseId = c.CourseId and p.GroupId = s.GroupId
) as t