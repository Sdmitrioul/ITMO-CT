select m.StudentId
from Marks m
         left join Plan p on m.CourseId = p.CourseId
         left join Lecturers l on l.LecturerId = p.LecturerId and l.LecturerName = :LecturerName
group by m.StudentId
having count(l.LecturerName) = (
    select count(pp.CourseId)
    from Plan pp inner join Lecturers ll on pp.LecturerId = ll.LecturerId and ll.LecturerName = :LecturerName
)