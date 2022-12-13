update Students as s
set Marks = s.Marks + (
    select count(n.CourseId)
    from NewMarks n
    where n.StudentId = s.StudentId
)
where True;