select StudentName, CourseName
from (select distinct CourseName, StudentName, StudentId
      from Students
          natural join Plan
          natural join Courses
      ) t
