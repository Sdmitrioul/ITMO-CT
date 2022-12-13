select s.StudentId, s.StudentName, g.GroupName
from Students s,
     Groups g
where s.GroupId = g.GroupId
  and s.GroupId in (
      select p.GroupId
      from Plan p
      where p.CourseId = :CourseId
    )
  and s.StudentId not in (
    select m.StudentId
    from Marks m
    where m.CourseId = :CourseId
);