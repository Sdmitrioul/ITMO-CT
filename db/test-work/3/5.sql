UPDATE RUNS AS R
SET ACCEPTED = 1
WHERE R.RUNID IN (SELECT RUNID
                  FROM RUNS
                           NATURAL JOIN
                       (SELECT SESSIONID, LETTER, MAX(SUBMITTIME) AS SUBMITTIME
                        FROM RUNS
                        GROUP BY SESSIONID, LETTER) QUERY);