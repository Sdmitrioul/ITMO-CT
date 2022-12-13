insert into Marks (StudentId, CourseId, Mark)
select n.StudentId, n.CourseId, n.Mark
from NewMarks n
where not exists(
    select m.StudentId, m.CourseId
    from Marks m
    where m.StudentId = n.StudentId
      and  n.CourseId = m.CourseId
    );