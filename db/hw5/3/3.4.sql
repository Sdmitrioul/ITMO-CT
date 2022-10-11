select StudentId, StudentName, GroupId
from Marks
    natural join Plan
    natural join Students
    natural join Lecturers
where Mark = :Mark and LecturerName = :LecturerName