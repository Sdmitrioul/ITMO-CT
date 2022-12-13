update Students as s
set Marks = (
    select count(m.CourseId)
    from Marks m
    where m.StudentId = s.StudentId
)
where True;