CREATE DATABASE ctd;
USE ctd;
CREATE TABLE `Groups`(group_id INT, group_no char(6));
CREATE TABLE Students(student_id int, name varchar(30), group_id int);
INSERT INTO `Groups` (group_id, group_no) VALUES 
    (1, 'M34371'), 
    (2, 'M34391');
INSERT INTO Students (student_id, name, group_id) VALUES 
    (1, 'Ilona Bozhe', 2),
    (2, 'Alex Slastin', 1),
    (3, 'Ivan Uss', 1);
SELECT group_id, group_no from `Groups`;
SELECT student_id, name, group_id from Students;
select name, group_no from Students natural join `Groups`;
SELECT Students.name, `Groups`.group_no from Students inner join `Groups` on Students.group_id = `Groups`.group_id;
update Students
    set group_id = 2 
    where student_id = 2;
update Students
    set group_id = 2 
    where name = 'Alex Slastin';
insert into `Groups` (group_id, group_no) values
    (1, 'M34381');
delete from `Groups` where group_no = 'M34381';
alter table `Groups`
    add constraint group_id_unique unique (group_id);
update Students set group_id = 5 where student_id = 1;
update Students set group_id = 1 where student_id = 1;
alter table Students add foreign key (group_id)
    references `Groups` (group_id);
