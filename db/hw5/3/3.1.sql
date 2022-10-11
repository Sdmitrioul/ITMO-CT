select StudentId, StudentName, GroupId
from (
    select StudentId
    from Marks
    where Marks.CourseId = :CourseId and Marks.Mark = :Mark
     ) R natural join Students
