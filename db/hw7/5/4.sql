create view StudentDebts(StudentId, Debts) as
select ss.StudentId, (
    select count(distinct p.CourseId)
    from Students s natural join Plan p
    where ss.StudentId = s.StudentId and
          not exists (
            select distinct m.CourseId
            from Marks m
            where s.StudentId = m.StudentId
              and p.CourseId = m.CourseId
            )
) as Debts
from Students ss
