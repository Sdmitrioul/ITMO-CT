------------------------
-- TABLES DECLARATION --
------------------------

CREATE TABLE USERS
(
    USER_ID    Int          NOT NULL
        CONSTRAINT USERS_PK PRIMARY KEY,
    USER_NAME  Varchar(128) NOT NULL,
    USER_EMAIL Varchar(128) NOT NULL
        CONSTRAINT USERS_EMAIL_UK UNIQUE
);

CREATE TABLE GROUPS
(
    GROUP_ID    Int          NOT NULL
        CONSTRAINT GROUPS_PK PRIMARY KEY,
    GROUP_NAME  Varchar(128) NOT NULL,
    DESCRIPTION Varchar(256) NOT NULL
);

CREATE TABLE PAYMENTS
(
    PAYMENT_ID Int          NOT NULL
        CONSTRAINT PAYMENTS_PK PRIMARY KEY,
    COMMENT    Varchar(128) NOT NULL,
    AMOUNT     Int          NOT NULL,
    SENDER     Int          NOT NULL
        CONSTRAINT PAYMENTS_FK_USER_SENDER REFERENCES USERS (USER_ID),
    RECEIVER   Int          NOT NULL
        CONSTRAINT PAYMENTS_FK_USER_RECEIVER REFERENCES USERS (USER_ID),
    GROUP_ID   Int          NOT NULL
        CONSTRAINT PAYMENTS_FK_GROUPS REFERENCES GROUPS (GROUP_ID)
);

CREATE TABLE GROUP_PARTICIPANTS
(
    GROUP_ID Int NOT NULL
        CONSTRAINT GROUP_PARTICIPANTS_FK_GROUPS REFERENCES GROUPS (GROUP_ID),
    USER_ID  Int NOT NULL
        CONSTRAINT GROUP_PARTICIPANTS_FK_USER_RECEIVER REFERENCES USERS (USER_ID),
    CONSTRAINT GROUP_PARTICIPANTS_PK PRIMARY KEY (GROUP_ID, USER_ID)
);

CREATE TABLE GROUP_INVITATIONS
(
    GROUP_ID Int NOT NULL
        CONSTRAINT GROUP_INVITATIONS_FK_GROUPS REFERENCES GROUPS (GROUP_ID),
    SENDER   Int NOT NULL
        CONSTRAINT GROUP_INVITATIONS_FK_USER_SENDER REFERENCES USERS (USER_ID),
    RECEIVER Int NOT NULL
        CONSTRAINT GROUP_INVITATIONS_FK_USER_RECEIVER REFERENCES USERS (USER_ID),
    CONSTRAINT GROUP_INVITATIONS_PK PRIMARY KEY (GROUP_ID, RECEIVER, SENDER)
);

CREATE TABLE EXPENSES
(
    EXPENSE_ID   Int          NOT NULL
        CONSTRAINT EXPENSES_PK PRIMARY KEY,
    EXPENSE_NAME Varchar(128) NOT NULL,
    GROUP_ID     Int          NOT NULL
        CONSTRAINT EXPENSES_FK_GROUP REFERENCES GROUPS (GROUP_ID),
    PAYER        Int          NOT NULL
        CONSTRAINT EXPENSES_FK_USERS REFERENCES USERS (USER_ID)
);

CREATE TABLE EXPENSES_PARTITIONS
(
    USER_ID    Int NOT NULL
        CONSTRAINT EXPENSES_PARTITIONS_FK_USERS REFERENCES USERS (USER_ID),
    EXPENSE_ID Int NOT NULL
        CONSTRAINT EXPENSES_PARTITIONS_FK_EXPENSES REFERENCES EXPENSES (EXPENSE_ID),
    AMOUNT     Int NOT NULL,
    CONSTRAINT EXPENSES_PARTITIONS_PK PRIMARY KEY (USER_ID, EXPENSE_ID)
);

-------------
-- INDICES --
-------------

-- Индекс на основной ключ для ускорения соединений и поиска по нему
CREATE INDEX USERS_PK_INDEX ON USERS USING HASH (USER_ID);

-- Индекс на основной ключ для ускорения соединений и поиска по нему
CREATE INDEX GROUPS_PK_INDEX ON GROUPS USING HASH (GROUP_ID);

-- Индекс на основной ключ для ускорения соединений и поиска по нему
CREATE INDEX EXPENSES_PK_INDEX ON EXPENSES USING HASH (EXPENSE_ID);

-- Индекс на основной ключ для ускорения соединений и поиска по нему
CREATE INDEX PAYMENTS_PK_INDEX ON PAYMENTS USING HASH (PAYMENT_ID);

--Индекс для поиска по почте
CREATE UNIQUE INDEX USER_EMAIL_U_INDEX ON USERS USING BTREE (USER_EMAIL);

--Индекс для поиска пользователя по имени
CREATE INDEX USER_NAME_INDEX ON USERS USING BTREE (USER_NAME);

--Индекс для поиска всех платежей отправителя в группе
CREATE INDEX USER_SENDER_PAYMENTS_INDEX ON PAYMENTS USING BTREE (SENDER, GROUP_ID);

--Индекс для поиска всех платежей получателя в группе
CREATE INDEX USER_RECEIVER_PAYMENTS_INDEX ON PAYMENTS USING BTREE (RECEIVER, GROUP_ID);

--Индекс для поиска всех плажей получателя
CREATE INDEX RECEIVER_PAYMENTS_INDEX ON PAYMENTS USING HASH (RECEIVER);

--Индекс для поиска всех плажей отправителя
CREATE INDEX SENDER_PAYMENTS_INDEX ON PAYMENTS USING HASH (SENDER);

--Индекс для поиска всех плажей в группе
CREATE INDEX GROUP_PAYMENTS_INDEX ON PAYMENTS USING HASH (GROUP_ID);

--Индекс для поиска стоимости участников траты
CREATE INDEX EXPENSES_MEMBERS_INDEX ON EXPENSES_PARTITIONS USING BTREE (USER_ID, EXPENSE_ID);

--Индекс для поиска всех участников траты
CREATE INDEX EXPENSES_PARTITIONS_MEMBERS_INDEX ON EXPENSES_PARTITIONS USING HASH (EXPENSE_ID);

--Индекс для поиска всех трат пользователя
CREATE INDEX EXPENSES_OF_PERSON_INDEX ON EXPENSES_PARTITIONS USING HASH (USER_ID);

--Индекс для поиска id группы по ее названию
CREATE INDEX FIND_GROUP_BY_NAME_INDEX ON GROUPS USING HASH (GROUP_NAME);

--Индекс для поиска групп в которых пользователь участник
CREATE INDEX FIND_PERSON_GROUPS_INDEX ON GROUP_PARTICIPANTS USING BTREE (USER_ID);

--Индекс для поиска участников группы
CREATE INDEX FIND_GROUP_MEMBERS ON GROUP_PARTICIPANTS USING BTREE (GROUP_ID);

--Индекс для проверки наличия приглашенного человека.
CREATE INDEX FIND_INVITED_PERSON_INDEX ON GROUP_INVITATIONS USING BTREE (RECEIVER, GROUP_ID);

--Индекс для проверки наличия пригляшающего человека в грууппе.
CREATE INDEX FIND_INVITING_PERSON_INDEX ON GROUP_INVITATIONS USING BTREE (SENDER, GROUP_ID);

--Индекс для поиска трат пользователя за которые он платил
CREATE INDEX FIND_EXPENSES_BY_USER_INDEX ON EXPENSES USING HASH (PAYER);

--Индекс для поиска трат за которые платил пользователь в группе
CREATE INDEX FIND_EXPENSE_INDEX ON EXPENSES USING BTREE (PAYER, GROUP_ID);

