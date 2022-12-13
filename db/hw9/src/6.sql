create or replace function FlightsStatistics(in _UserId integer,
                                             in _Pass varchar(64))
    returns table
            (
                FlightId           integer,
                CanReserve         boolean,
                CanBuy             boolean,
                FreeSeats          integer,
                ReservedSeatsCount integer,
                BoughtSeatsCount   integer
            )
as
$$
begin
    --Проверяет, что пользователь существует
    if (not CustomerExists(_UserId, _Pass)) then
        return;
    end if;
    return query (select f.FlightId                     as FlightId,
                         r.CanReserve                   as CanReserve,
                         b.CanBuy                       as CanBuy,
                         f.FreeSeats::integer           as FreeSeats,
                         rs.ReservedSeatsCount::integer as ReservedSeatsCount,
                         bs.BoughtSeatsCount::integer   as BoughtSeatsCount
                  from (select fs.FlightId, count(fs.SeatNo) as FreeSeats
                        from FreeSeats fs
                        group by fs.FlightId) f
                           natural join (select fs.FlightId,
                                                count(fs.SeatNo) > 0 as CanReserve
                                         from FreeSeats fs
                                         group by fs.FlightId) r
                           natural join (select r.FlightId,
                                                count(r.SeatNo) as ReservedSeatsCount
                                         from (select f.FlightId, SeatNo
                                               from Flights f
                                                        left join ReservedSeats bb
                                                                  on f.FlightId = bb.FlightId) r
                                         group by r.FlightId) rs
                           natural join (select r.FlightId,
                                                count(r.SeatNo) as BoughtSeatsCount
                                         from (select f.FlightId, SeatNo
                                               from Flights f
                                                        left join BoughtSeats bb
                                                                  on f.FlightId = bb.FlightId) r
                                         group by r.FlightId) bs
                           natural join (select r.FlightId,
                                                count(r.SeatNo) > 0 as CanBuy
                                         from (select s.FlightId, s.SeatNo
                                               from SeatsOnFlight s
                                               except
                                               select b.FlightId, SeatNo
                                               from Bookings b
                                               where b.BookingState = 'Bought'
                                                  or (
                                                           b.BookingState =
                                                           'Reserved'
                                                       and b.BookingTime +
                                                           interval '3 days' >=
                                                           now()
                                                       and b.Customerid !=
                                                           _UserId
                                                   )) r
                                         group by r.FlightId) b);
end;
$$
    language 'plpgsql';