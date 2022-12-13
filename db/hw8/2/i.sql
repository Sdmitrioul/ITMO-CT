-- Внешний ключ, значит ускорит соединение таблиц
create index students_fk_group_id on Students using hash (GroupId);

-- Связующий покрывающий индекс связующей таблички
create unique index marks_pk_s_and_c on Marks using btree (StudentId, CourseId);

-- Связующий покрывающий индекс связующей таблички
create index marks_key_c_and_s on Marks using btree (CourseId, StudentId);

-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае всегда
create unique index groups_pk_id on Groups using hash (GroupId);

-- Часто ищем по имени, есть ФЗ GroupName -> GroupId.
create unique index groups_pk_name on Groups using hash (GroupName);

-- Не все базы данных создают хеш на основной ключ
-- Хэш тк он будет работать быстрее дерева в данном
-- случае всегда, тем более это главный ключ
create unique index courses_pk_id on Courses using hash (CourseId);

-- :CourseName среди тех, у кого он есть
-- Ищем по имени, ускоряет нахождение pk.
create index courses_pk_name on Courses using hash (CourseName);