--Для покупки или первичного бронирования:
start transaction read only isolation level read committed;
select FreeSeats(:FlightId);
commit work;

--Для покупки или продления бронирования:
start transaction read only isolation level read committed;
select s.SeatNo
from SeatsOnFlight(:FlightId) s
except
select b.SeatNo
from Bookings b
where b.FlightId = :FlightId
  and (b.BookingState = 'Bought' or (
            b.BookingState = 'Reserved'
        and b.BookingTime + interval '3 days' >= now()
        and b.Customerid != :CustomerId
    ));
commit work;
