UPDATE RUNS
SET ACCEPTED = 1
WHERE RUNID IN (SELECT RUNID
                FROM RUNS
                         NATURAL JOIN
                     (SELECT SESSIONID, MAX(SUBMITTIME) AS SUBMITTIME
                      FROM RUNS
                      GROUP BY SESSIONID) QUERY);