merge into Marks m
using NewMarks nm
on m.StudentId = nm.StudentId
       and m.CourseId = nm.CourseId
when matched and nm.Mark > m.Mark then
    update set Mark = nm.Mark
when not matched then
    insert (StudentId, CourseId, Mark)
    values (nm.StudentId, nm.CourseId, nm.Mark);
