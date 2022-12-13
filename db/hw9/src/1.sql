create or replace function FreeSeats(in _FlightId integer,
                                     in _Time timestamp default now())
    returns table
            (
                SeatNo varchar(4)
            )
as
$$
begin
    return query (select s.SeatNo
                  from SeatsOnFlight(_FlightId) s
                  except
                  select b.SeatNo
                  from Bookings b
                  where b.FlightId = _FlightId
                    and (b.BookingState = 'Bought' or (
                              b.BookingState = 'Reserved'
                          and b.BookingTime + interval '3 days' >= _Time
                      )));
end;
$$
    language 'plpgsql';