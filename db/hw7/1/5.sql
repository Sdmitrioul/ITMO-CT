delete
from Students
where StudentId not in (
    select m.StudentId
    from Marks m
    group by m.StudentId
    having count(m.Mark) > 3
);