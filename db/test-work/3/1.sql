DELETE
FROM RUNS AS R
WHERE R.SESSIONID IN (SELECT S.SESSIONID
                      FROM SESSIONS S
                      WHERE S.CONTESTID = :ContestId);