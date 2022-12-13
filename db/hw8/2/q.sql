select avg(m.Mark)
from Students s
    natural join Marks m
where s.GroupId = (
    select g.GroupId
    from Groups g
    where g.GroupName = :GroupName
    )
  and m.CourseId = (
      select c.CourseId
      from Courses c
      where c.CourseName = :CourseName
    )
group by m.CourseId