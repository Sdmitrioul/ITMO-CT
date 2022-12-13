-- ДЗ-5.4.1 Информацию о студентах не имеющих оценки по :CourseName
-- среди всех
-- ДЗ-5.4.2. Информацию о студентах не имеющих оценки по :CourseName
-- среди тех, у кого он есть
-- ДЗ-5.5.2. ФИО студента и названия предметов которые у него есть
-- без оценки
-- Связующий покрывающий индекс связующей таблички
create unique index marks_pk_s_and_c on Marks using btree (StudentId, CourseId);

-- ДЗ-5.8.1. SumMark по :StudentId
-- ДЗ-5.8.2. SumMark для каждого студента (StudentName)
-- ДЗ-5.8.3. SumMark студентов каждой группы (GroupName)
-- Связующий покрывающий индекс связующей таблички
create index marks_key_s_and_m on Marks using btree (StudentId, Mark);

-- ДЗ-5.6.3 StudentId имеющих оценки по всем предметам :LecturerName
-- ДЗ-5.6.4 StudentId имеющих оценки по всем предметам :LecturerName,
-- которые он у него вёл
-- ДЗ-6.5.3. StudentId имеющих оценки по всем предметам :LecturerName
-- Связующий покрывающий индекс связующей таблички
create index marks_key_c_and_s on Marks using btree (CourseId, StudentId);

