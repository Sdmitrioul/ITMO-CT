create or replace function CheckBookings()
    returns trigger as
$$
begin
    if (exists(
            select FlightId, SeatNo
            from Bookings
            except
            select FlightId, SeatNo
            from SeatsOnFlight
        ))
    then
        raise exception 'Find extra seats %', now();
    end if;
    return new;
end;
$$
    language 'plpgsql';

create trigger NoExtraSeats
    after insert or update or delete
    on seats
execute procedure CheckBookings();

create trigger NoExtraSeats
    after insert or update or delete
    on Flights
execute procedure CheckBookings();

create trigger NoExtraSeats
    after insert or update
    on Bookings
execute procedure CheckBookings();

--Возвращает самолет при полете
create or replace function PlaneOfFlight(in _FlightId integer)
    returns integer as
$$
begin
    return (select PlaneId from Flights where FlightId = _FlightId);
end;
$$ language 'plpgsql';

--Возвращает доступные места в полете
create or replace function SeatsOnFlight(in _FlightId integer)
    returns table
            (
                SeatNo varchar(4)
            )
as
$$
declare
    _PlaneId integer := PlaneOfFlight(_FlightId);
begin
    return query (select s.SeatNo
                  from Seats s
                  where s.PlaneId = _PlaneId);
end;
$$
    language 'plpgsql';

--Возвращает существует ли покупатель
create or replace function CustomerExists(in _UserId integer, in _Pass varchar(64))
    returns boolean
as
$$
begin
    return exists(select customerid
                      from customers
                      where customerid = _UserId
                        and customerpass = _Pass);
end;
$$
    language 'plpgsql';

--Удаляет бронирование
create or replace procedure DeleteBooking(in _FlightId integer, in _SeatNo varchar(4))
as
$$
begin
    delete from Bookings b where b.FlightId = _FlightId and b.SeatNo = _SeatNo;
end;
$$
    language 'plpgsql';

--возвращает время полета
create or replace function GetTimeOfFlight(in _FlightId integer)
    returns timestamp
as
$$
begin
    return (select FlightTime from Flights where FlightId = _FlightId);
end;
$$
    language 'plpgsql';