select g.GroupName, sum(m.Mark) as SumMark
from Groups g
    left join Students s on g.GroupId = s.GroupId
    left join Marks m on s.StudentId = m.StudentId
group by g.GroupId, g.GroupName
;