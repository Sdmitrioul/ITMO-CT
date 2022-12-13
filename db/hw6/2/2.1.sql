select s.StudentId, s.StudentName, g.GroupName
from Students s,
     Groups g
where s.GroupId = g.GroupId;