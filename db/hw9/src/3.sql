create or replace function ExtendReservation(in _UserId integer,
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
    set BookingTime = time
    where flightid = _FlightId
      and seatno = _SeatNo
      and customerid = _UserId
      and bookingstate = 'Reserved'
      and bookingtime + interval '3 days' >= time;
    get diagnostics result = row_count;

    return result > 0;
end;
$$
    language 'plpgsql';
