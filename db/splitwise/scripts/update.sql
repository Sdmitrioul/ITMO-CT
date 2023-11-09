--Название: Пригласить человека в групппу
--read commited
--Неповторное чттение нас не волнует, тк мы если запись пропала, значит пользователь отказался и ему не нужна эта запись
CREATE OR REPLACE PROCEDURE INVITE_PERSON_TO_GROUP(_GROUP_ID Int,
                                                   _SENDER_ID Int,
                                                   _RECEIVER_ID Int)
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    IF (NOT EXISTS(SELECT RECEIVER
                   FROM GROUP_INVITATIONS
                   WHERE RECEIVER = _RECEIVER_ID
                     AND GROUP_ID = _GROUP_ID)) THEN
        INSERT INTO GROUP_INVITATIONS (GROUP_ID, SENDER, RECEIVER)
        VALUES (_GROUP_ID, _SENDER_ID, _RECEIVER_ID);
    END IF;
END;
$$;

--Название: пригласить человека в группу по email
--Serializable, но нам не ттак критично здесь ошибиться тк, если что нас защитит PK на ID,
--поэтому можем использовать ReadCommitted
CREATE OR REPLACE PROCEDURE INVITE_PERSON_TO_GROUP(_GROUP_ID Int,
                                                   _SENDER_ID Int,
                                                   _RECEIVER_EMAIL Varchar(128))
    LANGUAGE PLPGSQL
AS
$$
DECLARE
    ID Int = -1;
BEGIN
    SELECT USER_ID
    INTO ID
    FROM USERS
    WHERE USER_EMAIL == _RECEIVER_EMAIL;

    IF (ID != -1) THEN
        INSERT INTO GROUP_INVITATIONS (GROUP_ID, SENDER, RECEIVER)
        VALUES (_GROUP_ID, _SENDER_ID, ID);
    ELSE
        SELECT MAX(USER_ID)
        INTO ID
        FROM USERS;

        INSERT INTO USERS (USER_ID, USER_NAME, USER_EMAIL)
        VALUES (ID + 1, LEFT(_RECEIVER_EMAIL, STRPOS(_RECEIVER_EMAIL, '@') - 1),
                _RECEIVER_EMAIL);

        INSERT INTO GROUP_INVITATIONS (GROUP_ID, SENDER, RECEIVER)
        VALUES (_GROUP_ID, _SENDER_ID, ID + 1);
    END IF;
END;
$$;

--Название: Расчитаться
--RepeatableRead
CREATE OR REPLACE FUNCTION SETTLE_UP(_GROUP_ID Int, _SENDER_ID Int,
                                     _RECEIVER_ID Int,
                                     _MESSAGE Varchar(128) DEFAULT 'Thn') RETURNS Boolean
    LANGUAGE PLPGSQL
AS
$$
DECLARE
    _AMOUNT Int = 0;
    ID      Int = -1;
BEGIN
    SELECT COALESCE(SUM(AMOUNT)::Int, 0)
    INTO _AMOUNT
    FROM USER_TOTAL_BALANCE(_RECEIVER_ID, _GROUP_ID)
    WHERE USER_ID = _SENDER_ID;

    IF (_AMOUNT = 0 OR _AMOUNT < 0) THEN
        RETURN FALSE;
    ELSE
        SELECT MAX(PAYMENT_ID)
        INTO ID
        FROM PAYMENTS;

        INSERT INTO PAYMENTS (PAYMENT_ID, COMMENT, AMOUNT, SENDER, RECEIVER,
                              GROUP_ID)
        VALUES (ID + 1, _MESSAGE, AMOUNT, _SENDER_ID, _RECEIVER_ID, _GROUP_ID);

        RETURN TRUE;
    END IF;
END;
$$;

--Название: принять приглашение в группу
--RepeatableRead
CREATE OR REPLACE FUNCTION ACCEPT_INVITATION(_GROUP_ID Int, _RECEIVER_ID Int)
    RETURNS Boolean
    LANGUAGE PLPGSQL AS
$$
DECLARE
    RESULT Int;
BEGIN
    IF (EXISTS(SELECT SENDER
               FROM GROUP_INVITATIONS
               WHERE RECEIVER = _RECEIVER_ID
                 AND GROUP_ID = _GROUP_ID)) THEN
        INSERT INTO GROUP_PARTICIPANTS (GROUP_ID, USER_ID)
        VALUES (_GROUP_ID, _RECEIVER_ID);

        GET DIAGNOSTICS RESULT = ROW_COUNT;

        IF (RESULT > 0) THEN
            DELETE
            FROM GROUP_INVITATIONS
            WHERE GROUP_ID = _GROUP_ID
              AND RECEIVER = _RECEIVER_ID;
            RETURN TRUE;
        END IF;
        RETURN RESULT > 0;
    END IF;

    RETURN FALSE;
END;
$$;

--Название: отклонить приглашение в группу
--ReadUncommitted
CREATE OR REPLACE PROCEDURE DECLINE_INVITATION(_GROUP_ID Int, _RECEIVER_ID Int)
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    DELETE
    FROM GROUP_INVITATIONS
    WHERE GROUP_ID = _GROUP_ID
      AND _RECEIVER_ID = RECEIVER;
END;
$$;

-----------------
-- CONSTRAINTS --
-----------------

--Проверяет, что человек которого приглашают в группу, является членом этой группы и приглашаемый еще не участник
CREATE OR REPLACE FUNCTION CHECK_INVITATION_SENDER() RETURNS Trigger
AS
$$
BEGIN
    IF (NOT EXISTS(SELECT G.USER_ID
                   FROM GROUP_PARTICIPANTS AS G
                   WHERE G.GROUP_ID = NEW.GROUP_ID
                     AND G.USER_ID = NEW.SENDER)
        OR EXISTS(SELECT G.USER_ID
                  FROM GROUP_PARTICIPANTS AS G
                  WHERE G.GROUP_ID = NEW.GROUP_ID
                    AND G.USER_ID = NEW.RECEIVER)) THEN
        RAISE EXCEPTION 'Invitor to group is not member of the group or receiver already member!';
    ELSE
        RETURN NEW;
    END IF;
END;
$$
    LANGUAGE PLPGSQL;

DROP TRIGGER IF EXISTS CHECK_GROUP_INVITOR_TRIGGER ON GROUP_INVITATIONS;
CREATE TRIGGER CHECK_GROUP_INVITOR_TRIGGER
    AFTER INSERT OR UPDATE
    ON GROUP_INVITATIONS
    FOR EACH ROW
EXECUTE PROCEDURE CHECK_INVITATION_SENDER();

--Проверяет, что и получатель и отправитель денег из одной группы
CREATE OR REPLACE FUNCTION CHECK_SENDER_AND_RECEIVER_OF_PAYMENT() RETURNS Trigger
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    IF NOT EXISTS(SELECT USER_ID
                  FROM GROUP_PARTICIPANTS AS G
                  WHERE G.GROUP_ID = NEW.GROUP_ID
                    AND G.USER_ID = NEW.SENDER)
        OR
       NOT EXISTS(SELECT USER_ID
                  FROM GROUP_PARTICIPANTS AS G
                  WHERE G.GROUP_ID = NEW.GROUP_ID
                    AND G.USER_ID = NEW.RECEIVER) THEN
        RAISE EXCEPTION 'Receiver or sender of payment not in group!';
    ELSE
        RETURN NEW;
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS CHECK_SENDER_AND_RECEIVER_OF_PAYMENT_TRIGGER ON PAYMENTS;
CREATE TRIGGER CHECK_SENDER_AND_RECEIVER_OF_PAYMENT_TRIGGER
    AFTER INSERT OR UPDATE
    ON PAYMENTS
    FOR EACH ROW
EXECUTE PROCEDURE CHECK_SENDER_AND_RECEIVER_OF_PAYMENT();

--Проверяет, что пользователь не создавший группу, попадает, через  приглашение.
CREATE OR REPLACE FUNCTION CHECK_GROUP_PARTICIPANTS() RETURNS Trigger
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    IF EXISTS(SELECT GROUP_ID
              FROM GROUP_PARTICIPANTS GP
              WHERE GP.GROUP_ID = NEW.GROUP_ID
              GROUP BY GROUP_ID
              HAVING COUNT(USER_ID) > 1)
        AND NOT EXISTS(SELECT RECEIVER
                       FROM GROUP_INVITATIONS GG
                       WHERE GG.GROUP_ID = NEW.GROUP_ID
                         AND GG.RECEIVER = NEW.USER_ID) THEN
        RAISE EXCEPTION 'Group participant must be invited!';
    ELSE
        RETURN NEW;
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS CHECK_GROUP_PARTICIPANTS_TRIGGER ON GROUP_PARTICIPANTS;
CREATE TRIGGER CHECK_GROUP_PARTICIPANTS_TRIGGER
    AFTER INSERT OR UPDATE
    ON GROUP_PARTICIPANTS
    FOR EACH ROW
EXECUTE PROCEDURE CHECK_GROUP_PARTICIPANTS();

--Проверяет, что пользователь , который платит, является членом группы
CREATE OR REPLACE FUNCTION CHECK_EXPENSE_PAYER() RETURNS Trigger
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    IF NOT EXISTS(SELECT USER_ID
                  FROM GROUP_PARTICIPANTS GP
                  WHERE GP.GROUP_ID = NEW.GROUP_ID
                    AND GP.USER_ID = NEW.PAYER) THEN
        RAISE EXCEPTION 'Expense payer must be group member!';
    ELSE
        RETURN NEW;
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS CHECK_EXPENSE_PAYER_TRIGGER ON EXPENSES;
CREATE TRIGGER CHECK_EXPENSE_PAYER_TRIGGER
    AFTER INSERT OR UPDATE
    ON EXPENSES
    FOR EACH ROW
EXECUTE PROCEDURE CHECK_EXPENSE_PAYER();

--Проверяет, что участник траты, является участником группы, в которой эта трата совершается
CREATE OR REPLACE FUNCTION CHECK_EXPENSE_MEMBER() RETURNS Trigger
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    IF NOT EXISTS(SELECT USER_ID
                  FROM GROUP_PARTICIPANTS GP
                  WHERE GP.GROUP_ID = (SELECT GROUP_ID
                                       FROM EXPENSES E
                                       WHERE E.EXPENSE_ID = NEW.EXPENSE_ID)
                    AND GP.USER_ID = NEW.USER_ID) THEN
        RAISE EXCEPTION 'Expense member must be group member!';
    ELSE
        RETURN NEW;
    END IF;
END;
$$;

DROP TRIGGER IF EXISTS CHECK_EXPENSE_MEMBER_TRIGGER ON EXPENSES_PARTITIONS;
CREATE TRIGGER CHECK_EXPENSE_MEMBER_TRIGGER
    AFTER INSERT OR UPDATE
    ON EXPENSES_PARTITIONS
    FOR EACH ROW
EXECUTE PROCEDURE CHECK_EXPENSE_MEMBER();