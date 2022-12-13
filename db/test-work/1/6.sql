SELECT TEAMNAME
FROM TEAMS
WHERE TEAMID IN (SELECT TEAMID
                 FROM (SELECT TEAMID, SESSIONID
                       FROM SESSIONS
                       EXCEPT
                       SELECT TEAMID, SESSIONID
                       FROM SESSIONS
                                NATURAL JOIN RUNS
                       WHERE ACCEPTED = 1) AS TSTS);