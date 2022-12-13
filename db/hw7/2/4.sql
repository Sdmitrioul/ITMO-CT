update Students
set GroupId = (
    select g.GroupId
    from Groups g
    where g.GroupName = :GroupName
    )
where GroupId = (
    select g.GroupId
    from Groups g
    where g.GroupName = :FromGroupName
    );