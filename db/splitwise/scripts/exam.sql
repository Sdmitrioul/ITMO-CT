CREATE OR REPLACE VIEW GET_EXPENSE_AMOUNT AS
SELECT E.EXPENSE_ID, COALESCE(SUM(AMOUNT), 0) AS AMOUNT
FROM EXPENSES E
         LEFT JOIN EXPENSES_PARTITIONS EP ON E.EXPENSE_ID = EP.EXPENSE_ID
GROUP BY E.EXPENSE_ID;

CREATE VIEW GET_USER_DEBT_IN_GROUP AS
SELECT COALESCE(SUM(COALESCE(R.AMOUNT, 0)), 0) AS DEBT,
       R.USER_ID                               AS USER_ID,
       R.GROUP_ID                              AS GROUP_ID
FROM (SELECT COALESCE(SUM(COALESCE(EP.AMOUNT, 0)), 0) AS AMOUNT,
             EP.USER_ID                               AS USER_ID,
             EE.GROUP_ID                              AS GROUP_ID
      FROM (EXPENSES E
          LEFT JOIN GROUP_PARTICIPANTS GP USING (GROUP_ID)) EE
               LEFT JOIN EXPENSES_PARTITIONS EP ON EE.EXPENSE_ID = EP.EXPENSE_ID
      GROUP BY EP.USER_ID, EE.GROUP_ID
      UNION ALL
      SELECT - COALESCE(SUM(AMOUNT), 0) AS AMOUNT,
             E.PAYER                    AS USER_ID,
             E.GROUP_ID                 AS GROUP_ID
      FROM EXPENSES E
               LEFT JOIN EXPENSES_PARTITIONS EP ON E.EXPENSE_ID = EP.EXPENSE_ID
      GROUP BY E.PAYER, E.GROUP_ID) R
GROUP BY R.USER_ID, R.GROUP_ID;

CREATE VIEW GET_USER_STATISTIC_IN_GROUP AS
SELECT SUM(COALESCE(AMOUNT, 0))
FROM (GROUPS
    LEFT JOIN GROUP_PARTICIPANTS USING (GROUP_ID)) P
         LEFT JOIN EXPENSES USING (GROUP_ID)
         LEFT JOIN EXPENSES_PARTITIONS USING (EXPENSE_ID)
WHERE PAYER <> P.USER_ID
GROUP BY PAYER, P.USER_ID;