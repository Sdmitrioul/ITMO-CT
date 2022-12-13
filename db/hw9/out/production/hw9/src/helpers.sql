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