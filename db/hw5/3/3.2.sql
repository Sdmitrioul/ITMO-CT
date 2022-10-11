select StudentId, StudentName, GroupId
from (
         select StudentId
         from (
                select CourseId
                from Courses
                where Courses.CourseName = :CourseName
              ) F natural join Marks
         where Marks.Mark = :Mark
     ) R natural join Students