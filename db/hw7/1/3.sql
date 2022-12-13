delete
from Students
where StudentId not in (
    select m.StudentId
    from Marks m
    );