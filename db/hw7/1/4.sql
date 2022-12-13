delete
from Students
where StudentId in (
    select m.StudentId
    from Marks m
    group by m.StudentId
    having count(m.Mark) >= 3
    );