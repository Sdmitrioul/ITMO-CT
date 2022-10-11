select s.StudentId,
       s.StudentName,
       s.GroupId
from Plan p
         inner join Marks m on p.CourseId = m.CourseId
         inner join Students s on s.StudentId = m.StudentId
         inner join Lecturers l on l.LecturerId = p.LecturerId
where m.Mark = :Mark
  and l.LecturerName = :LecturerName