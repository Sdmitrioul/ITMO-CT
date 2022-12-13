-- psql (PostgreSQL) 14.5 (Homebrew)

create function CheckNewMark()
    returns trigger as
    $$
    begin
        update Marks m
        set Mark = old.Mark
        where m.CourseId = old.CourseId
          and m.StudentId = old.StudentId;
        return new;
    end;
    $$
    language 'plpgsql';

create trigger PreserveMarks
    after update of Mark on Marks
    for each row
    when ( old.Mark > new.Mark )
    execute procedure CheckNewMark();