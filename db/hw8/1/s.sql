-- ДЗ-5.1.1. Информация о студентах по :StudentId
-- ДЗ-5.2.1. Полная информация о студентах по :StudentId
-- ДЗ-5.8.1. Суммарный балл студентов по :StudentId
-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае
create unique index students_pk_id on Students using hash (StudentId);

-- ДЗ-5.2.1. Полная информация о студентах по :StudentId
-- ДЗ-5.3.2. Информацию о студентах с :Mark по предмету :CourseName
-- ДЗ-5.10. Статистика по студентам
-- Внешний ключ, значит ускорит соединение таблиц
create index students_fk_group_id on Students using hash (GroupId);

-- ДЗ-5.1.2. Информация о студентах по :StudentName
-- ДЗ-5.2.2. Полная информация о студентах по :StudentName
-- ДЗ-5.9.2. AvgMark для каждого студента (StudentName)
-- Хэш быстрее так как дерево будет медленнее.
create index students_sk_name on Students using hash (StudentName);

-- ДЗ-5.6.1. StudentId имеющих хотя бы одну оценку у
-- :LecturerName
-- ДЗ-5.6.2. StudentId не имеющих оценок у :LecturerName
-- ДЗ-5.6.3. StudentId имеющих оценки по всем предметам
-- GroupId, StudentId - можно использовать как таблица связи
create index students_t_gs on Students using btree (GroupId, StudentId);