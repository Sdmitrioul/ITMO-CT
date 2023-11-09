-- Название: сумма траты
-- Возвращяет сумму траты
CREATE OR REPLACE FUNCTION GET_EXPENSE_AMOUNT(_EXPENSE_ID Int) RETURNS Int
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN (SELECT SUM(AMOUNT)
            FROM EXPENSES_PARTITIONS EP
            WHERE EP.EXPENSE_ID = _EXPENSE_ID);
END;
$$;

-- Название: Потраченная сумма
-- Возвращяет сколько конкретный пользователь заплатил в группе
CREATE OR REPLACE FUNCTION GET_USER_SPENDING_IN_GROUP(_USER_ID Int, _GROUP_ID Int) RETURNS Int
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN (SELECT COALESCE(SUM(AMOUNT), 0)
            FROM EXPENSES_PARTITIONS
            WHERE EXPENSE_ID IN (SELECT EXPENSE_ID
                                 FROM EXPENSES E
                                 WHERE E.PAYER = _USER_ID
                                   AND E.GROUP_ID = _GROUP_ID));
END;
$$;

-- Название долг группе
--Возвращяет сколько пользователь должен в группе
CREATE OR REPLACE FUNCTION GET_USER_DEBT_IN_GROUP(_USER_ID Int, _GROUP_ID Int) RETURNS Int
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN (SELECT COALESCE(SUM(AMOUNT), 0)
            FROM EXPENSES_PARTITIONS EP
            WHERE EP.USER_ID = _USER_ID
              AND EP.EXPENSE_ID IN (SELECT EXPENSE_ID
                                    FROM EXPENSES E
                                    WHERE E.GROUP_ID = _GROUP_ID))
        - GET_USER_SPENDING_IN_GROUP(_USER_ID, _GROUP_ID);
END;
$$;

-- Название: взятые деньги
-- Возвращяет сколько и кто одолжиловал у пользователя из группы
CREATE OR REPLACE FUNCTION GET_PEOPLE_FOR_WHOM_USER_PAID(_USER_ID Int, _GROUP_ID Int)
    RETURNS Table
            (
                USER_ID Int,
                AMOUNT  Int
            )
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN QUERY (SELECT EP.USER_ID AS USER_ID, SUM(EP.AMOUNT)::Int AS AMOUNT
                  FROM EXPENSES_PARTITIONS EP
                  WHERE EP.USER_ID != _USER_ID
                    AND EP.EXPENSE_ID IN (SELECT E.EXPENSE_ID
                                          FROM EXPENSES E
                                          WHERE E.PAYER = _USER_ID
                                            AND E.GROUP_ID = _GROUP_ID)
                  GROUP BY EP.USER_ID);
END;
$$;

--Название: финансовый баланс в группе для пользователя
CREATE OR REPLACE FUNCTION GET_USER_STATISTIC_IN_GROUP(_USER_ID Int, _GROUP_ID Int)
    RETURNS Table
            (
                USER_ID Int,
                AMOUNT  Int
            )
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN QUERY (SELECT UIA.USER_ID, SUM(UIA.AMOUNT)::Int AS AMOUNNT
                  FROM (SELECT EP.USER_ID          AS USER_ID,
                               SUM(EP.AMOUNT)::Int AS AMOUNT
                        FROM EXPENSES_PARTITIONS EP
                        WHERE EP.USER_ID != _USER_ID
                          AND EP.EXPENSE_ID IN (SELECT E.EXPENSE_ID
                                                FROM EXPENSES E
                                                WHERE E.PAYER = _USER_ID
                                                  AND E.GROUP_ID = _GROUP_ID)
                        GROUP BY EP.USER_ID
                        UNION
                        SELECT PAYER AS USER_ID, - SUM(EP.AMOUNT)::Int
                        FROM (SELECT EEPP.AMOUNT, EEPP.EXPENSE_ID
                              FROM EXPENSES_PARTITIONS EEPP
                              WHERE EEPP.USER_ID = _USER_ID) EP
                                 NATURAL JOIN (SELECT PAYER, EXPENSE_ID
                                               FROM EXPENSES
                                               WHERE PAYER != _USER_ID
                                                 AND GROUP_ID = _GROUP_ID) AS PEI
                        GROUP BY PAYER) AS UIA
                  GROUP BY UIA.USER_ID);
END;
$$;

-- Название: Баланс пользователя в группе
-- Возвращяет текущий баланс пользователя в группе
CREATE OR REPLACE FUNCTION USER_TOTAL_BALANCE(_USER_ID Int, _GROUP_ID Int)
    RETURNS Table
            (
                USER_ID Int,
                AMOUNT  Int
            )
    LANGUAGE PLPGSQL
AS
$$
BEGIN
    RETURN QUERY (SELECT UIAUIA.USER_ID, SUM(UIAUIA.AMOUNT)::Int AS AMOUNT
                  FROM (SELECT R.USER_ID, R.AMOUNT
                        FROM GET_USER_STATISTIC_IN_GROUP(_USER_ID, _GROUP_ID) R
                        UNION
                        SELECT P.SENDER AS USER_ID, - P.AMOUNT AS AMOUNT
                        FROM PAYMENTS P
                        WHERE P.RECEIVER = _USER_ID
                          AND P.GROUP_ID = _GROUP_ID
                        UNION
                        SELECT PP.RECEIVER AS USER_ID, PP.AMOUNT AS AMOUNT
                        FROM PAYMENTS PP
                        WHERE PP.SENDER = _USER_ID
                          AND PP.GROUP_ID = _GROUP_ID) AS UIAUIA
                  GROUP BY UIAUIA.USER_ID);
END;
$$;

CREATE VIEW EXPENSES_AMOUNT AS
SELECT EXPENSE_ID,
       EXPENSE_NAME,
       PAYER,
       GROUP_ID,
       GET_EXPENSE_AMOUNT(EXPENSE_ID) AS AMOUNT
FROM EXPENSES;

--Название: приглашенные пользоваттели в группу
CREATE FUNCTION GET_INVITED_TO_GROUP_PEOPLE(_GROUP_ID Int)
    RETURNS Table
            (
                USER_ID Int
            )
AS
$$
BEGIN
    RETURN QUERY (SELECT USER_ID
                  FROM GROUP_INVITATIONS
                  WHERE GROUP_ID = _GROUP_ID);
END;
$$;