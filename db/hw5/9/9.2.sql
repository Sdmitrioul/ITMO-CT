select s.StudentName, r.AvgMark
from Students s left join (
    select m.StudentId as StudentId, avg(cast(m.Mark as real)) as AvgMark
    from Marks m
    group by m.StudentId
) r on r.StudentId = s.StudentId;