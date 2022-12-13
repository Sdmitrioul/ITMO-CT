select StudentId, StudentName, GroupId
from Students s
where s.StudentId in (
    select StudentId
    from Marks m
    where m.Mark = :Mark and m.CourseId = :CourseId
);