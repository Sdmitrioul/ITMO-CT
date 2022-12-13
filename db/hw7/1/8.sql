delete
from Students
where StudentId not in (
    select r.StudentId
    from (
             select s.StudentId, p.CourseId
             from Students s natural join Plan p
             except
             select m.StudentId, m.CourseId
             from Marks m
         ) r
    group by r.StudentId
    having count(distinct CourseId) > 2
);
