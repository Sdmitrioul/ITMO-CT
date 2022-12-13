create or replace function Reserve(in _UserId integer, in _Pass varchar(64),
                                   in _FlightId integer, in _SeatNo varchar(4))
    returns boolean
as
$$
declare
    time timestamp := now();
begin
    --Проверяет, что пользователь существует и место свободно
    if (not CustomerExists(_UserId, _Pass) or
        _SeatNo not in (select SeatNo from FreeSeats(_FlightId, time))) then
        return false;
    end if;

    if (time >= GetTimeOfFlight(_FlightId)) then
        return false;
    end if;

    call DeleteBooking(_FlightId, _SeatNo);

    insert into Bookings (FlightId, SeatNo, BookingState, CustomerId,
                          BookingTime)
    values (_FlightId, _SeatNo, 'Reserved', _UserId, time);

    return true;
end;
$$
    language 'plpgsql';