CREATE TABLE IF NOT EXISTS TEAMS
(
    TEAMID   Integer      NOT NULL,
    TEAMNAME Varchar(128) NOT NULL,
    CONSTRAINT "pk_Teams" PRIMARY KEY (TEAMID)
);

CREATE TABLE IF NOT EXISTS CONTESTS
(
    CONTESTID   Integer      NOT NULL,
    CONTESTNAME Varchar(128) NOT NULL,
    CONSTRAINT "pk_Contests" PRIMARY KEY (CONTESTID)
);

CREATE TABLE IF NOT EXISTS PROBLEMS
(
    CONTESTID   Integer      NOT NULL,
    LETTER      Char         NOT NULL,
    PROBLEMNAME Varchar(128) NOT NULL,
    CONSTRAINT "pk_Problems" PRIMARY KEY (CONTESTID, LETTER),
    CONSTRAINT "fk_Problems_Contests" FOREIGN KEY (CONTESTID) REFERENCES CONTESTS (CONTESTID)
);

CREATE TABLE IF NOT EXISTS SESSIONS
(
    SESSIONID Integer   NOT NULL,
    TEAMID    Integer   NOT NULL,
    CONTESTID Integer   NOT NULL,
    START     Timestamp NOT NULL,
    CONSTRAINT "pk_Sessions" PRIMARY KEY (SESSIONID),
    CONSTRAINT "fk_Sessions_Teams" FOREIGN KEY (TEAMID) REFERENCES TEAMS (TEAMID),
    CONSTRAINT "fk_Sessions_Contests" FOREIGN KEY (CONTESTID) REFERENCES CONTESTS (CONTESTID)
);

CREATE TABLE IF NOT EXISTS RUNS
(
    RUNID      Integer  NOT NULL,
    SESSIONID  Integer  NOT NULL,
    LETTER     Char     NOT NULL,
    SUBMITTIME Integer  NOT NULL,
    ACCEPTED   Smallint NOT NULL, --only 0,1
    CONSTRAINT "pk_Runs" PRIMARY KEY (RUNID),
    CONSTRAINT "fk_Runs_Sessions" FOREIGN KEY (SESSIONID) REFERENCES SESSIONS (SESSIONID)
);
