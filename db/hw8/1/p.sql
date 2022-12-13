-- ДЗ-5.3.3. Информацию о студентах с :Mark по предмету :LecturerId
-- ДЗ-5.3.4. Информацию о студентах с :Mark по предмету :LecturerName
-- ДЗ-5.3.5. Информацию о студентах с :Mark по предмету :LecturerId
-- Связующий покрывающий индекс связующей таблички
create index plan_c_and_g_to_l on Plan using btree (CourseId, GroupId);

-- ДЗ-5.4.2. Информацию о студентах не имеющих оценки по :CourseName
-- среди тех, у кого он есть
-- ДЗ-5.5.1. ФИО студента и названия предметов которые у него есть
-- по плану
-- ДЗ-5.5.2. ФИО студента и названия предметов которые у него есть
-- без оценки
-- Связующий покрывающий индекс связующей таблички
create index plan_g_and_c_to_l on Plan using btree (GroupId, CourseId);

-- ДЗ-5.3.3. Информацию о студентах с :Mark по предмету :LecturerId
-- ДЗ-5.3.5. Информацию о студентах с :Mark по предмету :LecturerId
-- ДЗ-5.6.3. StudentId имеющих оценки по всем предметам :LecturerName
-- Связующий покрывающий индекс связующей таблички
create index plan_l_and_c_to_g on Plan using btree (LecturerId, CourseId);