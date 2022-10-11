# HW4

StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark

StudentId           -> StudentName, GroupId
GroupId             -> GroupName
CourseId            -> CourseName
LectureId           -> LecturerName
GroupId, CourseId   -> LecturerId
StudentId, CourseId -> Mark

Ключи (CourseId, StudentId).

1

Отношение находится в 1нф, тк оно атрибуты атомарны, у них отношения есть ключ(из прошлого дз - (CourseId, StudentId)), нет повторяющихся групп.

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => (StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark).

2

Условия для 2нф

1нф
Неключевые атрибуты функционально зависят от ключа в целом (не от части ключа)
Неключевой атрибут – не входящий в ключ

Для выполнения второго условия усовершенствуем ФЗ.

StudentId           -> StudentName, GroupId, GroupName
CourseId            -> CourseName
GroupId, CourseId   -> LecturerId, LecturerName
StudentId, CourseId -> Mark

(StudentId, StudentName, GroupId, GroupName, CourseId, CourseName, LecturerId, LecturerName, Mark) => (StudentId, StudentName, GroupId, GroupName); (CourseId, CourseName); (GroupId, CourseId, LecturerId, LecturerName); (StudentId, CourseId, Mark).

3

Условия для 3нф

2нф
Неключевые атрибуты непосредственно (не транзитивно) зависят от ключей

Для выполнения второго уусловия усовершенствуем ФЗ.

StudentId           -> StudentName, GroupId -> GroupName
CourseId            -> CourseName
GroupId, CourseId   -> LecturerId           -> LecturerName
StudentId, CourseId -> Mark

(StudentId, StudentName, GroupId, GroupName) => (StudentId, StudentName, GroupId); (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId, LecturerName) => (GroupId, CourseId, LecturerId); (LecturerId, LecturerName).
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).

4

Условия НФБК

В каждой нетривиальной функциональной зависимости X→Y, X является надключом

Рассмотрим существующие Отношения:

(StudentId, StudentName, GroupId)
(GroupId, GroupName)
(CourseId, CourseName)
(GroupId, CourseId, LecturerId)
(LecturerId, LecturerName)
(StudentId, CourseId, Mark)

Тк в каждом отношении, кроме второго есть лишь одна ФЗ, они все (Кроме 2) ууже находятся в НФБК, тк ключ(а значит надключ) каждой из них находится слева их ФЗ.
А в отношенеии (GroupId, GroupName), где ФЗ (GroupId -> GroupName, GroupName -> GroupId) и GroupId, GroupName являются ключами.

(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId). 
(LecturerId, LecturerName) => (LecturerId, LecturerName).
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).

5

По Тореме ДФ2 (GroupId, GroupName), (CourseId, CourseName), (LecturerId, LecturerName) находятся в 4нф тк ключи там простые.

Для (GroupId, CourseId, LecturerId), отношение находится в НФБК и существует только одна нетрвиальная МЗ, которая описана ФЗ, и так как может быть что у группы отсутствует лектор при отсутствии у группы данного курса отношение уже в 4нф.

Для (StudentId, CourseId, Mark), отношение находится в НФБК и существует только одна нетрвиальная МЗ, которая описана ФЗ, и так как может быть что у студента отсутствует оценка при отсутствии у него данного курса отношение уже в 4нф.



(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId). 
(LecturerId, LecturerName) => (LecturerId, LecturerName).
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).

6

По Тореме ДФ1 (GroupId, GroupName), (CourseId, CourseName), (LecturerId, LecturerName) находятся в 5нф тк ключи там простые.

Для (GroupId, CourseId, LecturerId), уже в 5нф, тк ЗС единственно, нетривиально и не является МЗ, так как ни по курсу нельзя определить лектора, как и по группе, аналогично лектор, те при попытке разбиение получится некоректным.

Для (StudentId, CourseId, Mark), уже в 5нф, тк ЗС единственно, нетривиально и не является МЗ, так как ни по студенту, ни по курсу нельзя однозначно определить оценку, аналогично со стороны оценки, те при попытке разбиение получится некоректным.

(GroupId, GroupName) => (GroupId, GroupName).
(CourseId, CourseName) => (CourseId, CourseName).
(GroupId, CourseId, LecturerId) => (GroupId, CourseId, LecturerId). 
(LecturerId, LecturerName) => (LecturerId, LecturerName).
(StudentId, CourseId, Mark) => (StudentId, CourseId, Mark).
