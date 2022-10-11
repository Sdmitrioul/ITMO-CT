select GroupName, CourseName
from (
         select GroupId, CourseId
         from Students s inner join Marks m on true
         except
         select GroupId, CourseId
         from (select CourseId, ss.StudentId, GroupId
               from Students ss inner join Marks mm on true
               except
               select CourseId, sss.StudentId, GroupId
               from Students sss natural join Marks mmm
              ) rr
     ) r natural join Courses cccc natural join Groups gggg