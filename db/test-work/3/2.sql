DELETE
FROM RUNS AS RR
WHERE RR.SESSIONID IN (SELECT SESSIONID
                       FROM (SESSIONS S
                           NATURAL JOIN
                           (SELECT TEAMID
                            FROM TEAMS T
                            WHERE T.TEAMNAME = :TeamName) AS TT));