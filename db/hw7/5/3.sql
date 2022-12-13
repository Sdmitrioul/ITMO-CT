create view Debts(StudentId, Debts) as
select r.StudentId, count(r.CourseId) as Debts
from (
    select StudentId, CourseId
    from Students s natural join Plan p
    except
    select StudentId, CourseId
    from Marks m
     ) r
group by r.StudentId
having count(r.CourseId) > 0;