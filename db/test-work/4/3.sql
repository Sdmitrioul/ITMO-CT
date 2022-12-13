SELECT LETTER
FROM RUNS
         NATURAL JOIN (SELECT SESSIONID
                       FROM SESSIONS
                       WHERE CONTESTID = :ContestId) AS RR
WHERE ACCEPTED = 1
GROUP BY LETTER
HAVING COUNT(*) = (SELECT COUNT(*) AS RES
                   FROM RUNS
                            NATURAL JOIN (SELECT SESSIONID
                                          FROM SESSIONS
                                          WHERE CONTESTID = :ContestId) AS RR
                   WHERE ACCEPTED = 1
                   GROUP BY LETTER
                   ORDER BY RES DESC
                   LIMIT 1);