--Хранит самолеты
create table if not exists Planes
(
    PlaneId integer not null,
    constraint pk_Planes primary key (PlaneId)
);

--Хранит полеты
create table if not exists Flights
(
    FlightId   integer   not null,
    FlightTime timestamp not null,
    PlaneId    integer   not null,
    constraint pk_Flights primary key (FlightId),
    constraint fk_Flights_PlaneId
        foreign key (PlaneId)
            references Planes (PlaneId)
);

--Хранит места в самолетах
create table if not exists Seats
(
    PlaneId integer    not null,
    SeatNo  varchar(4) not null,
    constraint pk_Seats primary key (PlaneId, SeatNo),
    constraint fk_Seats_PlaneId
        foreign key (PlaneId)
            references Planes (PlaneId)
);

--Хранит покупателей
create table if not exists Customers
(
    CustomerId   integer      not null,
    CustomerName varchar(128) not null,
    CustomerPass varchar(64)  not null,
    constraint pk_Customers primary key (CustomerId),
    constraint uk_Customers_Pass unique (CustomerPass)
);

--Типы букинга
create type BookingType as enum ('Reserved', 'Bought');

--Показывает места на полете
create or replace view SeatsOnFlight as
select f.FlightId, s.SeatNo
from Seats s
         natural join Flights f;

--Хранит бронирования на полетах
create table if not exists Bookings
(
    FlightId     integer     not null,
    SeatNo       varchar(4)  not null,
    BookingState BookingType not null,
    CustomerId   integer     not null,
    BookingTime  timestamp   not null,

    constraint pk_Bookings primary key (FlightId, SeatNo),
    constraint fk_Booking_FlightId foreign key (FlightId) references Flights (FlightId),
    constraint fk_Bookings_CustomerId foreign key (CustomerId) references Customers (CustomerId)
);

--Показывает купленные места на полетах
create or replace view BoughtSeats as
select FlightId, SeatNo
from Bookings
where BookingState = 'Bought';

--Показывает зарезервированные места на полетах
create or replace view ReservedSeats as
select FlightId, SeatNo
from Bookings
where BookingState = 'Reserved'
  and BookingTime + interval '3 days' >= now();

--Показывает свободные места на полетах
create or replace view FreeSeats as
select FlightId, SeatNo
from SeatsOnFlight
except
select FlightId, SeatNo
from Bookings
where BookingState = 'Bought'
   or (
            BookingState = 'Reserved'
        and BookingTime + interval '3 days' >= now()
    );
