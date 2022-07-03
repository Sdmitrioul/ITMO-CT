package mateByRook.player;

import mateByRook.*;
import mateByRook.coordinates.Move;

public class Winner implements Player {
    private int[][] cor;
    /*1. White king
      2. White rook
      3. Black king
    */
    private Move next;
    private boolean shown = true;
    private boolean brw0nRow = false;
    private boolean brw = false;
    private boolean rookWasNearKing = false;


    @Override
    public Move move(Position position, Colour colour) throws InputException {
        this.cor = position.getCoordinates();
        if (shown) {
            //System.out.println(3);
            return winningMove();
        } else {
            //System.out.println(4);
            shown = true;
            return next;
        }
    }

    private Move winningMove() {
        /*int brwPos = brwPositionOnRow();
        if (Math.abs(brwPos) == 1) {
            return brwMoveOnRow(brwPos);
        } else if (Math.abs(brwPos) == 2) {
            return makeRookCloserOnRow(brwPos);
        }*/
        if (brw) {
            //System.out.println(1);
            return brwMove(brwPosition());
        } else if (brw0nRow) {
            //System.out.println(2);
            return brwMoveOnRow(brwPositionOnRow());
        } else {
            int a = brwPosition();
            if (a != 0) {
                //System.out.println(true);
                brw = true;
                return brwMove(a);
            }
            a = brwPositionOnRow();
            if (a != 0) {
                //System.out.println(true);
                brw0nRow = true;
                return brwMoveOnRow(a);
            }
            //System.out.println(false);
            return makeBrw();
        }
    }

    private Move brwMove(int dy) {
        int rc = cor[1][1];
        int rr = cor[1][0];
        int bc = cor[2][1];
        int br = cor[2][0];
        int wc = cor[0][1];
        int wr = cor[0][0];
        if (rookAttacked()) {
            if (!becomeAttacked(rr, 7 - rc)) {
                return new Move(rr, rc, rr, 7 - rc);
            } else {
                if (!becomeAttacked(rr, 0)) {
                    return new Move(rr, rc, rr, 0);
                } else {
                    return new Move(rr, rc, rr, 7);
                }
            }
        } else if (bc == wc && Math.abs(br - wr) == 2) {
            return new Move(rr, rc, rr + dy, rc);
        } else if (horsePosition()) {
            if (bc <= 3) {
                if (rc == 7) {
                    return new Move(rr, rc, rr, rc - 1);
                } else {
                    return new Move(rr, rc, rr, 7);
                }
            } else {
                if (rc == 0) {
                    return new Move(rr, rc, rr, rc + 1);
                } else {
                    return new Move(rr, rc, rr, 0);
                }
            }
        } else {
            return makeKingCloser(dy);
        }
    }

    private Move brwMoveOnRow(int dx) {
        int rc = cor[1][1];
        int rr = cor[1][0];
        int bc = cor[2][1];
        int br = cor[2][0];
        int wc = cor[0][1];
        int wr = cor[0][0];
        if (rookAttacked()) {
            if (!becomeAttacked(7 - rr, rc)) {
                return new Move(rr, rc, 7 - rr, rc);
            } else {
                if (!becomeAttacked(0, rc)) {
                    return new Move(rr, rc, 0, rc);
                } else {
                    return new Move(rr, rc, 7, rc);
                }
            }
        } else if (br == wr && Math.abs(bc - wc) == 2) {
            return new Move(rr, rc, rr, rc + dx);
        } else if (horsePositionOnRow()) {
            if (br <= 3) {
                if (rr == 7) {
                    return new Move(rr, rc, rr - 1, rc);
                } else {
                    return new Move(rr, rc, 7, rc);
                }
            } else {
                if (rr == 0) {
                    return new Move(rr, rc, rr + 1, rc);
                } else {
                    return new Move(rr, rc, 0, rc);
                }
            }
        } else {
            return makeKingCloserOnRow(dx);
        }
    }

    private Move rookToKing() {
        if (cor[0][0] != cor[1][0] && cor[0][1] != cor[1][1]) {
            if (!rookBecomeAttacked(cor[0][0], cor[1][1])) {
                return new Move(cor[1][0], cor[1][1], cor[0][0], cor[1][1]);
            } else {
                return new Move(cor[1][0], cor[1][1], cor[1][0], cor[0][1]);
            }
        } else if (cor[0][0] == cor[1][0]) {
            if (cor[0][1] - cor[1][1] > 0) {
                return new Move(cor[1][0], cor[1][1], cor[1][0], cor[0][1] - 1);
            } else {
                return new Move(cor[1][0], cor[1][1], cor[1][0], cor[0][1] + 1);
            }
        } else {
            if (cor[0][0] - cor[1][0] > 0) {
                return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[0][1]);
            } else {
                return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[0][1]);
            }
        }
    }

    private Move makeBrw() {
        if (!rookWasNearKing) {
            if (rookIsNearHisKing()) {
                rookWasNearKing = true;
                return makeBrw();
            } else {
                return rookToKing();
            }
        }
        if (Math.abs(cor[0][0] - cor[2][0]) > 1 && cor[1][0] == cor[0][0]) {
            if (cor[0][0] - cor[2][0] > 0) {
                return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[1][1]);
            } else {
                return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[1][1]);
            }
        } else if (Math.abs(cor[0][1] - cor[2][1]) > 1 && cor[1][1] == cor[0][1]) {
            if (cor[0][1] - cor[2][1] > 0) {
                return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] - 1);
            } else {
                return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] + 1);
            }
        } else {
            return allOnLineOrNear();
        }
    }

    private Move allOnLineOrNear() {
        if (cor[0][1] == cor[2][1]) {
            if (cor[0][0] > cor[2][0]) {
                if (rightCoord(cor[1][0], cor[1][1] + 1)) {
                    next = new Move(cor[1][0], cor[1][1] + 1, cor[1][0] - 2, cor[1][1] + 1);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] + 1);
                } else {
                    next = new Move(cor[1][0], cor[1][1] - 1, cor[1][0] - 2, cor[1][1] - 1);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] - 1);
                }
            } else {
                if (rightCoord(cor[1][0], cor[1][1] + 1)) {
                    next = new Move(cor[1][0], cor[1][1] + 1, cor[1][0] + 2, cor[1][1] + 1);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] + 1);
                } else {
                    next = new Move(cor[1][0], cor[1][1] - 1, cor[1][0] + 2, cor[1][1] - 1);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] - 1);
                }
            }
        } else if (cor[0][0] == cor[2][0]) {
            if (cor[0][1] > cor[2][1]) {
                if (rightCoord(cor[1][0] + 1, cor[1][1])) {
                    next = new Move(cor[1][0]  + 1, cor[1][1], cor[1][0] + 1, cor[1][1] - 2);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[1][1]);
                } else {
                    next = new Move(cor[1][0]  - 1, cor[1][1], cor[1][0] - 1, cor[1][1] - 2);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[1][1]);
                }
            } else {
                if (rightCoord(cor[1][0] + 1, cor[1][1])) {
                    next = new Move(cor[1][0]  + 1, cor[1][1], cor[1][0] + 1, cor[1][1] + 2);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[1][1]);
                } else {
                    next = new Move(cor[1][0]  - 1, cor[1][1], cor[1][0] - 1, cor[1][1] + 2);
                    shown = false;
                    return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[1][1]);
                }
            }
        } else if (cor[0][1] == cor[1][1]) {
            if (cor[1][0] < cor[2][0]) {
                if (cor[2][1] > cor[0][1]) {
                    shown = false;
                    next = new Move(cor[1][0], cor[1][1] + 1, cor[1][0] + 2, cor[1][1] + 1);
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] + 1);
                } else {
                    shown = false;
                    next = new Move(cor[1][0], cor[1][1] - 1, cor[1][0] + 2, cor[1][1] - 1);
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] - 1);
                }
            } else {
                if (cor[2][1] > cor[0][1]) {
                    shown = false;
                    next = new Move(cor[1][0], cor[1][1] + 1, cor[1][0] - 2, cor[1][1] + 1);
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] + 1);
                } else {
                    shown = false;
                    next = new Move(cor[1][0], cor[1][1] - 1, cor[1][0] - 2, cor[1][1] - 1);
                    return new Move(cor[1][0], cor[1][1], cor[1][0], cor[1][1] - 1);
                }
            }
        } else/* if (cor[0][0] == cor[1][0]) */ {
            if (cor[1][1] < cor[2][1]) {
                if (cor[2][0] > cor[0][0]) {
                    shown = false;
                    next = new Move(cor[1][0] + 1, cor[1][1], cor[1][0] + 1, cor[1][1] + 2);
                    return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[1][1]);
                } else {
                    shown = false;
                    next = new Move(cor[1][0] - 1, cor[1][1], cor[1][0] - 1, cor[1][1] + 2);
                    return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[1][1]);
                }
            } else {
                if (cor[2][0] > cor[0][0]) {
                    shown = false;
                    next = new Move(cor[1][0] + 1, cor[1][1], cor[1][0] + 1, cor[1][1] - 2);
                    return new Move(cor[1][0], cor[1][1], cor[1][0] + 1, cor[1][1]);
                } else {
                    shown = false;
                    next = new Move(cor[1][0] - 1, cor[1][1], cor[1][0] - 1, cor[1][1] - 2);
                    return new Move(cor[1][0], cor[1][1], cor[1][0] - 1, cor[1][1]);
                }
            }
        }
    }

    private Move makeKingCloser(int dy) {
        if (cor[0][1] - cor[2][1] > 0) {
            if (cor[0][0] + 2 * dy == cor[2][0]) {
                return new Move(cor[0][0], cor[0][1], cor[0][0], cor[0][1] - 1);
            } else {
                return new Move(cor[0][0], cor[0][1], cor[0][0] + dy, cor[0][1] - 1);
            }
        } else {
            if (cor[0][0] + 2 * dy == cor[2][0]) {
                return new Move(cor[0][0], cor[0][1], cor[0][0], cor[0][1] + 1);
            } else {
                return new Move(cor[0][0], cor[0][1], cor[0][0] + dy, cor[0][1] + 1);
            }
        }
    }

    private Move makeKingCloserOnRow(int dx) {
        if (cor[0][0] - cor[2][0] > 0) {
            if (cor[0][1] + 2 * dx == cor[2][1]) {
                return new Move(cor[0][0], cor[0][1], cor[0][0] - 1, cor[0][1]);
            } else {
                return new Move(cor[0][0], cor[0][1], cor[0][0] - 1, cor[0][1] + dx);
            }
        } else {
            if (cor[0][1] + 2 * dx == cor[2][1]) {
                return new Move(cor[0][0], cor[0][1], cor[0][0] + 1, cor[0][1]);
            } else {
                return new Move(cor[0][0], cor[0][1], cor[0][0] + 1, cor[0][1] + dx);
            }
        }
    }

    private int brwPosition() {
        if (cor[0][0] < cor[1][0] && cor[1][0] + 1 == cor[2][0]) {
            return 1;
        } else if (cor[0][0] > cor[1][0] && cor[1][0] - 1 == cor[2][0]) {
            return -1;
        } else if (cor[0][0] > cor[1][0] && cor[1][0] > cor[2][0]) {
            return -2;
        } else if (cor[0][0] < cor[1][0] && cor[1][0] < cor[2][0]) {
            return 2;
        } else {
            return 0;
        }
    }

    private int brwPositionOnRow() {
        if (cor[0][1] < cor[1][1] && cor[1][1] + 1 == cor[2][1]) {
            return 1;
        } else if (cor[0][1] > cor[1][1] && cor[1][1] - 1 == cor[2][1]) {
            return -1;
        } else if (cor[0][1] > cor[1][1] && cor[1][1] > cor[2][1]) {
            return -2;
        } else if (cor[0][1] < cor[1][1] && cor[1][1] < cor[2][1]) {
            return 2;
        } else {
            return 0;
        }
    }

    private boolean rookAttacked() {
        return Math.abs(cor[1][0] - cor[2][0]) <= 1 && Math.abs(cor[1][1] - cor[2][1]) <= 1;
    }

    private boolean rookBecomeAttacked(int row, int column) {
        return Math.abs(row - cor[2][0]) <= 1 && Math.abs(column - cor[2][1]) <= 1;
    }

    private boolean horsePosition() {
        return (cor[0][1] + 1 == cor[2][1] || cor[0][1] - 1 == cor[2][1]) && (cor[0][0] + 2 == cor[2][0] || cor[0][0] - 2 == cor[2][0]);
    }

    private boolean horsePositionOnRow() {
        return (cor[0][1] + 2 == cor[2][1] || cor[0][1] - 2 == cor[2][1]) && (cor[0][0] + 1 == cor[2][0] || cor[0][0] - 1 == cor[2][0]);
    }

    private boolean becomeAttacked(int row, int column) {
        return Math.abs(row - cor[2][0]) <= 1 && Math.abs(column - cor[2][1]) <= 1;
    }

    private boolean rookIsNearHisKing() {
        return (Math.abs(cor[0][0] - cor[1][0]) == 1 && Math.abs(cor[0][1] - cor[1][1]) == 0) || (Math.abs(cor[0][0] - cor[1][0]) == 0 && Math.abs(cor[0][1] - cor[1][1]) == 1);
    }

    private boolean rightCoord(int row, int column) {
        return row >= 0 && column >= 0 && row < 8 && column < 8;
    }
}
