select g.GroupName, avg(cast(r.AvgMark as real)) as AvgAvgMark
from Groups g
         left join (
            select s.StudentName, rr.AvgMark, s.GroupId
            from Students s left join (
                select m.StudentId as StudentId, avg(cast(m.Mark as real)) as AvgMark
                from Marks m
                group by m.StudentId
                ) rr on rr.StudentId = s.StudentId
            ) r  on g.GroupId = r.GroupId
group by g.GroupId, g.GroupName
;