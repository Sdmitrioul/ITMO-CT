SELECT PROBLEMNAME
FROM PROBLEMS P
WHERE EXISTS(
              SELECT S.SESSIONID, S.TEAMID, S.CONTESTID, R.LETTER
              FROM SESSIONS S,
                   RUNS R
              WHERE R.SESSIONID = S.SESSIONID
                AND P.LETTER = R.LETTER
                AND P.CONTESTID = S.CONTESTID
                AND R.ACCEPTED = 1
          )