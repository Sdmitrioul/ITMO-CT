-- psql (PostgreSQL) 14.5 (Homebrew)

create or replace function NoExtraMarks()
    returns trigger as
$$
begin
    if (exists(
            select StudentId, CourseId
            from Marks
            except
            select s.StudentId, p.CourseId
            from Students s
                     natural join Plan p
        )) then
        raise exception 'Find extra marks %', now();
    end if;
    return new;
end;
$$
    language 'plpgsql';

create trigger NoExtraMarks
    after insert or update
    on Marks
execute procedure NoExtraMarks();

create trigger NoExtraMarks
    after delete or update
    on Students
execute procedure NoExtraMarks();

create trigger NoExtraMarks
    after delete or update
    on Plan
execute procedure NoExtraMarks();