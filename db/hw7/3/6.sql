update Students as ss
set Debts = (
    select count(distinct p.CourseId)
    from Plan p
    where p.GroupId = ss.GroupId
      and not exists(
            select m.StudentId, m.CourseId, m.Mark
            from Marks m
            where m.StudentId = ss.StudentId
              and m.CourseId = p.CourseId
            )
    )
where True;