update Students
set GroupId = coalesce((
    select g.GroupId
    from Groups g
    where g.GroupName = :GroupName
), GroupId)
where GroupId = (
    select g.GroupId
    from Groups g
    where g.GroupName = :FromGroupName
    );