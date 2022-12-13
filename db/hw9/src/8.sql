create or replace procedure CompressSeats(in _FlightId integer)
as
$$
declare
    cursorSeats cursor for
        select SeatNo
        from SeatsOnFlight(_FlightId)
        order by cast(lpad(seatno, 4, '0') as char(4));
    cursorBought cursor for
        select SeatNo
        from Bookings b
        where b.FlightId = _FlightId
          and b.BookingState = 'Bought'
        order by cast(lpad(seatno, 4, '0') as char(4)) for update;
    cursorReserved cursor for
        select SeatNo
        from Bookings b
        where b.FlightId = _FlightId
          and b.BookingState = 'Reserved'
          and b.BookingTime + interval '3 days' >= now()
        order by cast(lpad(seatno, 4, '0') as char(4)) for update;
    tmp varchar(4);
begin
    --Удаляем пустые брони
    delete
    from Bookings b
    where b.FlightId = _FlightId
      and b.BookingState = 'Reserved'
      and b.BookingTime + interval '3 days' < now();

    open cursorSeats;

    for _ in cursorBought
        loop
            fetch cursorSeats into tmp;
            update bookings set SeatNo = tmp where current of cursorBought;
        end loop;

    for _ in cursorReserved
        loop
            fetch cursorSeats into tmp;
            update bookings set SeatNo = tmp where current of cursorReserved;
        end loop;
end;
$$
    language 'plpgsql';