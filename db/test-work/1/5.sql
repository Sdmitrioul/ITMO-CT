SELECT TEAMNAME
FROM TEAMS T
WHERE T.TEAMID IN
      (SELECT TEAMID
       FROM (SELECT TEAMID, CONTESTID
             FROM CONTESTS
                      NATURAL JOIN TEAMS
             EXCEPT
             SELECT TEAMID, CONTESTID
             FROM SESSIONS
                      NATURAL JOIN (SELECT SESSIONID
                                    FROM RUNS R
                                    WHERE R.ACCEPTED = 1) AS RR) AS RRR);