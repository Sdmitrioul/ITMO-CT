create or replace function BuyFree(
    in _FlightId integer,
    in _SeatNo varchar(4))
    returns boolean
as
$$
declare
    time timestamp := now();
begin
    --Проверяет, что самолет не улетел
    if (time >= GetTimeOfFlight(_FlightId)) then
        return false;
    end if;

    if (_SeatNo not in (select SeatNo from FreeSeats(_FlightId, time))) then
        return false;
    end if;

    insert into Bookings (FlightId, SeatNo, BookingState, CustomerId,
                          BookingTime)
    values (_FlightId, _SeatNo, 'Bought', null, time);

    return true;
end;
$$
    language 'plpgsql';