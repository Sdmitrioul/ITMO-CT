select m.StudentId
from Marks m
    natural join Plan p
    natural join Lecturers l
    natural join Students s
where LecturerName = :LecturerName
group by m.StudentId, l.LecturerId
having count(l.LecturerName) = (
    select count(CourseId)
    from Lecturers ll
        natural join Plan pp
        natural join Students ss
    where StudentId = m.StudentId
      and LecturerId = l.LecturerId
)