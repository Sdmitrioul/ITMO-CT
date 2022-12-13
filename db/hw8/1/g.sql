-- ДЗ-5.2.1. Полная информация о студентах по :StudentId
-- ДЗ-5.2.2. Полная информация о студентах по :StudentName
-- ДЗ-5.8.1. Суммарный балл студентов по :StudentId
-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае всегда
create unique index groups_pk_id on Groups using hash (GroupId);

-- ДЗ-5.8.3. SumMark студентов каждой группы (GroupName)
-- ДЗ-6.1.2. Информация о студентах по :GroupName
-- ДЗ-7.1.2. Удаление студентов по :GroupName
-- Часто ищем по имени, есть ФЗ GroupName -> GroupId.
create unique index groups_pk_name on Groups using hash (GroupName);