-- Индексируем оценки, чтобы искать по ним в диапазоне
create index marks_indd_mark_and_id on Marks using btree (Mark, StudentId);