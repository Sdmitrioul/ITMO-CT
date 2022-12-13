delete
from Students
where GroupId in (
    select g.GroupId
    from Groups g
    where g.GroupName = :GroupName
);