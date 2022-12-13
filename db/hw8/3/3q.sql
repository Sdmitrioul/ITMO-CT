-- Вывести список действующих преподавателей в алфавитном порядке
select LecturerName
from Lecturers natural join Plan
order by LecturerName