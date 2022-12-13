-- ДЗ-5.3.4. Информацию о студентах с :Mark по предмету
-- :LecturerName
-- ДЗ-5.6.2. StudentId не имеющих оценок у :LecturerName
-- ДЗ-5.6.3. StudentId имеющих оценки по всем предметам :LecturerName
-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае всегда, тем более это главный ключ
create unique index lecturers_pk_id on Lecturers using hash (LecturerId);

-- ДЗ-5.3.4. Информацию о студентах с :Mark по предмету :LecturerName
-- ДЗ-5.6.1. StudentId имеющих хотя бы одну оценку у :LecturerName
-- ДЗ-5.6.2. StudentId не имеющих оценок у :LecturerName
-- :CourseName среди тех, у кого он есть
-- Ищем по имени, ускоряет нахождение pk.
create index lecturers_pk_name on Lecturers using hash (LecturerName);