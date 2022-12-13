create or replace function FlightStat(in _UserId integer,
                                      in _Pass varchar(64),
                                      in _FlightId integer)
    returns table
            (
                CanReserve         boolean,
                CanBuy             boolean,
                FreeSeats          integer,
                ReservedSeatsCount integer,
                BoughtSeatsCount   integer
            )
as
$$
declare
    canBuy              boolean;
    canReserve          boolean;
    _FreeSeats          integer;
    _ReservedSeatsCount integer;
    _BoughtSeatsCount   integer;
begin
    --Проверяет, что пользователь существует
    if (not CustomerExists(_UserId, _Pass)) then
        return;
    end if;
    if (not exists(select FlightId
                   from Flights
                   where FlightId = _FlightId)) then
        return;
    end if;

    _FreeSeats = (select count(*) from FreeSeats(_FlightId));

    _ReservedSeatsCount = (select count(SeatNo)
                           from Bookings
                           where flightid = _FlightId
                             and BookingState =
                                 'Reserved'
                             and BookingTime +
                                 interval '3 days' >=
                                 now());

    _BoughtSeatsCount = (select count(SeatNo)
                         from Bookings
                         where flightid = _FlightId
                           and BookingState =
                               'Bought');

    canReserve = _FreeSeats > 0;

    canBuy = (select count(r.SeatNo) > 0
              from (select s.SeatNo
                    from SeatsOnFlight(_FlightId) s
                    except
                    select b.SeatNo
                    from Bookings b
                    where FlightId = _FlightId
                      and (bookingstate = 'Bought'
                        or (
                                       b.BookingState =
                                       'Reserved'
                                   and b.BookingTime +
                                       interval '3 days' >=
                                       now()
                                   and b.Customerid !=
                                       _UserId
                               ))) r);

    return
        query (select canReserve          as CanReserve,
                      canBuy              as CanBuy,
                      _FreeSeats          as FreeSeats,
                      _BoughtSeatsCount   as BoughtSeatsCount,
                      _ReservedSeatsCount as ReservedSeatsCount);
end;
$$
    language 'plpgsql';