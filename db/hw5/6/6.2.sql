select StudentId
from Students except
select distinct s.StudentId
from Students s
         inner join Marks m on m.StudentId = s.StudentId
         inner join Plan p on p.GroupId = s.GroupId and m.CourseId = p.CourseId
         inner join Lecturers l on l.LecturerId = p.LecturerId
where l.LecturerName = :LecturerName