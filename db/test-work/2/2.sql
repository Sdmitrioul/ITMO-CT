SELECT TEAMNAME
FROM TEAMS
WHERE TEAMID IN (SELECT TEAMID
                 FROM (SELECT SESSIONID
                       FROM RUNS
                       WHERE LETTER = :Letter
                         AND ACCEPTED = 1) AS RS
                          NATURAL JOIN (SELECT SESSIONID, TEAMID
                                        FROM SESSIONS
                                        WHERE CONTESTID = :ContestId) AS RR);