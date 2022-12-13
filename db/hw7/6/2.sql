-- psql (PostgreSQL) 14.5 (Homebrew)

create or replace function OnSameMarks()
    returns trigger as
    $$
    begin
    if (exists(
        select s.StudentId, r.CourseId
        from Students s
            natural join (
                select distinct GroupId, CourseId
                from Students natural join Marks
        ) r
        except
        select distinct s.StudentId, m.CourseId
        from Students s natural join Marks m
        )) then
            raise exception 'Find extra marks %', now();
    end if;
    return new;
    end;
    $$
    language 'plpgsql';

create trigger SameMarks
    after update or insert or delete
    on Marks
    execute procedure OnSameMarks();

create trigger SameMarks
    after update or insert or delete
    on Students
    execute procedure OnSameMarks();
