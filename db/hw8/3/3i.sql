-- btree имеет упорядоченную структуру, поэтому ускорит запрос
create unique index on Lecturers using btree (LecturerId, LecturerName)