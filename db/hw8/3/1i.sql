-- Btree имеет организованную структуру, поэтому это ускорит запрос.
create index students_id_g_and_s on Students using btree (GroupId, StudentName)