select g.GroupName, avg(cast(m.Mark as real)) as AvgMark
from Groups g
         left join Students s on g.GroupId = s.GroupId
         left join Marks m on s.StudentId = m.StudentId
group by g.GroupId, g.GroupName
;