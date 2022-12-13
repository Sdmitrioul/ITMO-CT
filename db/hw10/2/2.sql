--Купить без регистрации
start transaction read write isolation level read committed;
select Buyfree(:FlightId, :SeatNo);
commit work;

--Выкупить забронированное место
start transaction read write isolation level read committed;
select Buyreserved(:UserId, :Pass, :FlightId, :SeatNo);
commit work;

--Продлить бронь
start transaction read write isolation level read committed;
select Extendreservation(:UserId, :Pass, :FlightId, :SeatNo);
commit work;

--Забронировать первично
start transaction read write isolation level snapshot;
select Reserve(:UserId, :Pass, :FlightId, :SeatNo);
commit work;
