-- Выводит студентов из группы упорядоченных по имени студента
select StudentId, StudentName, GroupId
from Students
where GroupId = :GroupId
order by StudentName