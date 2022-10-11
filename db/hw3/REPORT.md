# HW3

StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark

1

StudentId           -> StudentName, GroupId
GroupId             -> GroupName
GroupName           -> GroupId
CourseId            -> CourseName
LecturerId          -> LecturerName
GroupId, CourseId   -> LecturerId
StudentId, CourseId -> Mark

2

*Общий принцип, пробуем убрать один элемент из атрибутов и посмотреть на замыкание оставшихся, будет ли оно равно искомому мн-ву атрибутов, если да то этот атрибут не входит в ключ, если нет, то он входит в ключ.
1. StudentId точно входит в ключ. (Точно будет отсутствовать StudentName).
2. CourseId точно входит в ключ. (Точно без него не будет Mark).
3. Остальные в замыкании дают нужное множество.
4. Тк замыкание (StudentId, CourseId) дает множество всех атрибутов - это точно надключ, а тк из него не выкинуть не один из атрибутов это ключ.
5. Других ключей нет, тк они должны включать в себя (StudentId, CourseId), а значит они не будут минимальными.


(StudentId, CourseId)

3

GroupId, CourseId
GroupId, CourseId, GroupName
GroupId, CourseId, GroupName, CourseName
GroupId, CourseId, GroupName, CourseName, LecturerId
GroupId, CourseId, GroupName, CourseName, LecturerId, LecturerName

StudentId, CourseId
StudentId, CourseId, StudentName, GroupId
StudentId, CourseId, StudentName, GroupId, GroupName
StudentId, CourseId, StudentName, GroupId, GroupName, CourseName
StudentId, CourseId, StudentName, GroupId, GroupName, CourseName, LecturerId
StudentId, CourseId, StudentName, GroupId, GroupName, CourseName, LecturerId, LecturerName
StudentId, CourseId, StudentName, GroupId, GroupName, CourseName, LecturerId, LecturerName, Mark

StudentId, LecturerId
StudentId, LecturerId, StudentName, GroupId
StudentId, LecturerId, StudentName, GroupId, GroupName
StudentId, LecturerId, StudentName, GroupId, GroupName, LecturerName

4

Расщепление правых частей.
Единственное правило ФЗ содержащее более одного атрибута с права: StudentId -> StudentName, GroupId <=> StudentId -> StudentName; StudentId -> GroupId.

StudentId           -> StudentName
StudentId           -> GroupId
GroupId             -> GroupName
GroupName           -> GroupId
CourseId            -> CourseName
LecturerId          -> LecturerName
GroupId, CourseId   -> LecturerId
StudentId, CourseId -> Mark

У нас есть только две ФЗ с несколькими атрибутами слева - это GroupId, CourseId   -> LecturerId, StudentId, CourseId -> Mark. Ни один из левых атрибутов мы не можем удалить, тк замыкание будет не включать атрибут с правой части.

Мы не можем удалить ни одну ФЗ, тк нет повторяющихся ФЗ по правому атрибуту, следовательно их замыкания не будут равны искомому.


