update Marks as m
set Mark = (
    select nm.Mark
    from NewMarks nm
    where nm.StudentId = m.StudentId
      and  m.CourseId = nm.CourseId
    )
where exists(
        select nm.StudentId, nm.CourseId
        from NewMarks nm
        where nm.StudentId = m.StudentId
          and  m.CourseId = nm.CourseId
    );