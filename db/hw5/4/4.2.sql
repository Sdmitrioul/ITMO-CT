select s.StudentId, s.StudentName, s.GroupId
from Plan p
    inner join Courses c on c.CourseId = p.CourseId
    inner join Students s on s.GroupId = p.GroupId
where c.CourseName = :CourseName
except
select s.StudentId, s.StudentName, s.GroupId
from Marks m
         inner join Courses c on c.CourseId = m.CourseId
         inner join Students s on s.StudentId = m.StudentId
where c.CourseName = :CourseName