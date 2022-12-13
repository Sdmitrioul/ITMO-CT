select s.StudentName, c.CourseName
from Students s,
     Courses c,
     (
         select m.StudentId,
                m.CourseId
         from Marks as m
         union
         select s.StudentId,
                p.CourseId
         from Students s,
              Plan as p
         where s.GroupId = p.GroupId
     ) st
where st.StudentId = s.StudentId and st.CourseId = c.CourseId