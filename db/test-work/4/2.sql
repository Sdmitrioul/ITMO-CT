SELECT TEAMID, COUNT(*) AS SOLVED
FROM (SELECT DISTINCT TEAMID, CONTESTID, LETTER
      FROM SESSIONS
               NATURAL JOIN RUNS
      WHERE ACCEPTED = 1) R
GROUP BY TEAMID;