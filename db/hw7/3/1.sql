update Students
set Marks = (
    select count(m.CourseId)
    from Marks m
    where m.StudentId = :StudentId
    )
where StudentId = :StudentId;
