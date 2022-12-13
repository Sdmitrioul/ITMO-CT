-- ДЗ-5.4.2. Информацию о студентах не имеющих оценки по
-- :CourseName среди тех, у кого он есть
-- ДЗ-5.5.2. ФИО студента и названия предметов которые у него есть
-- без оценки
-- ДЗ-5.5.3. ФИО студента и названия предметов которые у него есть,
-- но не 4 или 5
-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае всегда, тем более это главный ключ
create unique index courses_pk_id on Courses using hash (CourseId);

-- ДЗ-5.3.2. Информацию о студентах с :Mark по предмету :CourseName
-- ДЗ-5.4.1. Информацию о студентах не имеющих оценки по :CourseName
-- ДЗ-5.4.2. Информацию о студентах не имеющих оценки по
-- :CourseName среди тех, у кого он есть
-- Ищем по имени, ускоряет нахождение pk.
create index courses_pk_name on Courses using hash (CourseName);