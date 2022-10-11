select StudentId, StudentName, GroupName
from Students natural join Groups
where Students.StudentId = :StudentId