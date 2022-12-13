create view AllMarks(StudentId, Marks) as
select s.StudentId, count(m.CourseId) as Marks
from Students s
    natural left join (
        select mm.StudentId, mm.CourseId
        from Marks mm
        union all
        select nm.StudentId, nm.CourseId
        from NewMarks nm
        ) m
group by s.StudentId;