create or replace function BuyReserved(in _UserId integer,
                                       in _Pass varchar(64),
                                       in _FlightId integer,
                                       in _SeatNo varchar(4))
    returns boolean
as
$$
declare
    time   timestamp := now();
    result integer;
begin
    --Проверяет, что пользователь существует
    if (not CustomerExists(_UserId, _Pass)) then
        return false;
    end if;

    --Проверяет, что самолет не улетел
    if (time >= GetTimeOfFlight(_FlightId)) then
        return false;
    end if;

    update Bookings
    set BookingTime  = time,
        BookingState = 'Bought'
    where FlightId = _FlightId
      and SeatNo = _SeatNo
      and CustomerId = _UserId
      and BookingState = 'Reserved'
      and BookingTime + interval '3 days' >= time;
    get diagnostics result = row_count;

    return result > 0;
end;
$$
    language 'plpgsql';