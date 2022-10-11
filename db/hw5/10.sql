select
    tt.StudentId,
    tt.Total as Total,
    COALESCE(tp.Passed, 0) as Passed,
    tt.Total - COALESCE(tp.Passed, 0) as Failed
from (
         select StudentId, count(distinct p.CourseId) as Total
         from Students s
                  left join Plan p on s.GroupId = p.GroupId
         group by s.StudentId
     ) tt left join (
        select ss.StudentId, count(distinct pp.CourseId) as Passed
        from Students ss natural join Marks mm natural join Plan pp
        group by ss.StudentId
         ) tp on tt.StudentId = tp.StudentId;

