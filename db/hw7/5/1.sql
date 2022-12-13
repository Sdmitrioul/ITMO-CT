create view StudentMarks(StudentId, Marks) as
select s.StudentId, count(m.CourseId) as Marks
from Students s natural left join Marks m
group by s.StudentId;