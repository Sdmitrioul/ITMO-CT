-- Идентификаторы студентов, у которых есть оценка меньше 3.
select distinct StudentId
from Marks
where Mark < 3;