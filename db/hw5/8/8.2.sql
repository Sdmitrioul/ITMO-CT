select s.StudentName, r.SumMark
from Students s left join (
    select m.StudentId as StudentId, sum(m.Mark) as SumMark
    from Marks m
    group by m.StudentId
     ) r on r.StudentId = s.StudentId;