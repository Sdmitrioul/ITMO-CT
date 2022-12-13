update Students as s
set Marks = (
    select count(distinct m.CourseId)
    from Marks m
    where m.StudentId = s.StudentId
    )
where True;