"use strict";

function getRandomInt(max) {
    return Math.floor(Math.random() * Math.floor(max));
}

const RESULT = {
    WIN: 2,
    LOOSE: 0,
    DRAW: 1,
    DOUBLE: 5,
    UKNOWN: -1
};

function createEmptyBoard() {
    let board = [];
    board[0] = [];
    for (let i = 0; i < 8; i++) {
        for (let j = 0; j < 8; j++) {
            board[i].push(E);
        }
        board.push([]);
    }
    board.pop();
    board[3][3] = B;
    board[3][4] = W;
    board[4][3] = W;
    board[4][4] = B;
    return board;
}

const WIN = "WIN";
const LOOSE = "LOOSE";
const DRAW = "DRAW";
const UKNOWN = "UKNOWN";
const DOUBLE = "DOUBLE";
const B = "B";
const E = "*";
const W = "W";

function opposite(t) {
    return t === B ? W : B;
}

const cordsTranslate = {
    "A" : 0,
    "B" : 1,
    "C" : 2,
    "D" : 3,
    "E" : 4,
    "F" : 5,
    "G" : 6,
    "H" : 7
};

function Coordinates(first, second) {
    this.first = first;
    this.second = second;
}

Coordinates.prototype.getRow = function () {
    return this.second;
};

Coordinates.prototype.getColumn = function () {
    return this.first
};

function cords(string) {
    let mas = string.split(" ");
    if (mas.length === 1) {
        return new Coordinates(corTranslate.mas[0].charAt(0).toUpperCase(), parseInt(mas[0].substr(1, 1)) - 1)
    } else {
        return new Coordinates(corTranslate.mas[0].toUpperCase(), parseInt(mas[mas.length - 1]) - 1)
    }
}

function Player (move) {
    this.makeMove = move;
}

Player.prototype.move = function (board, cell) {
    return this.makeMove(board, cell);
};

function HumanPlayer() {
    Player.call(this, function (board, cell) {
        println("Position: ");
        println(board.toString());
        let string = readLine("Insert coordinats (Pattern: 'Column Row')");
        return cords(string);
    })
}

function StupidPlayer() {
    Player.call(this, function (board, cell) {
        for (let i = 0; i < 8; i++) {
            for (let j = 0; j < 8; j++) {
                let c = new Coordinates(i, j);
                if (board.isValid(c)) {
                    return c;
                }
            }
        }
    })
}

function RandomPlayer() {
    Player.call(this, function (board, cell) {
        while (true) {
            let i = getRandomInt(8);
            let j = getRandomInt(8);
            let c = new Coordinates(i, j);
            if (board.isValid(c)) {
                return c;
            }
        }
    })
}

function Game(l, Player1, Player2) {
    this.l = l;
    this.Player1 = Player1;
    this.Player2 = Player2;
}

Game.prototype.log = function (message) {
    if (this.l) {
        println(message);
    }
};

Game.prototype.play = function (board) {
    while (true) {
        let result1 = this.move(board, this.player1, 1);
        if (result1 !== -1) {
            return result1;
        }
        let result2 = this.move(board, this.player2, 2);
        if (result2 !== -1) {
            return result2;
        }
    }
};

Game.prototype.move = function (board, player, no) {
    let  move = player.move(board, board.getCell());
    let result = board.makeMove(move);
    if (result === 5) {
        result = this.move(board, player, no);
    }
    this.log("Player " + no + " move");
    this.log("Position:\n" + board.toString());
    if (RESULT[result] === 2) {
        this.log("Player " + no + " won");
        return no;
    } else if (RESULT[result] === 0) {
        this.log("Player " + no + " lose");
        return 3 - no;
    } else if (RESULT[result] === 1) {
        this.log("Draw");
        return 0;
    } else {
        return -1;
    }
};

function ReversiBoad () {
    this.board = createEmptyBoard();
    this.turn = B;
}

ReversiBoad.prototype.makeMove = function(move) {
    if (!this.isValid(move)) {
        return LOOSE;
    }

    this.paint(move);

    if (!this.haveValidMoves()) {
        this.turn = opposite(this.turn);
        if (!this.haveValidMoves()) {
            if (this.countCl(this.turn) > this.countCl(opposite(this.turn))) {
                return LOOSE;
            } else if (this.countCl(this.turn) < this.countCl(opposite(this.turn))) {
                return WIN;
            } else {
                return DRAW;
            }
        }
    }
    this.turn = opposite(this.turn);
    if (!this.haveValidMoves()) {
        this.turn = opposite(this.turn);
        return DOUBLE;
    }
    return UKNOWN;
};

ReversiBoad.prototype.paint = function(move) {
    this.board[move.getRow()][move.getColumn()] = this.turn;
    for (let i = -1; i < 2; i++) {
        for (let j = -1; j < 2; j++) {
            if (onVector(move, i, j)) {
                let row = move.getRow() + i;
                let column = move.getColumn() + j;
                while (this.board[row][column] !== this.turn) {
                    this.board[row][column] = this.turn;
                }
            }
        }
    }
};

ReversiBoad.prototype.countCl = function(t) {
    let count = 0;
    for (let i = 0; i < 8; i++) {
        for (let j = 0; j < 8; j++) {
            count = this.board[i][j] === t ? count + 1 : count;
        }
    }
    return count;
};

ReversiBoad.prototype.haveValidMoves = function() {
    for (let i = 0; i < 8; i++) {
        for (let j = 0; j < 8; j++) {
            let move = new Coordinates(i, j);
            if (this.isValid(move)) {
                return true;
            }
        }
    }
    return false;
};

ReversiBoad.prototype.getCell = function() {
    return this.turn;
};

ReversiBoad.prototype.isValid = function(move) {
    if (this.board[move.getRow()][move.getColumn()] !== E) {
        return false;
    }
    let checker = false;
    for (let i = -1; i < 2; i++) {
        for (let j = -1; j < 2; j++) {
            checker = checker || this.onVector(move, i, j);
        }
    }
    return checker;
};

ReversiBoad.prototype.onVector = function(move, dx, dy) {
    let opposite = this.turn === B ? W : B;
    let row = move.getRow();
    let column = move.getColumn();
    while (row + dx < 8 && column + dy < 8 && row + dx > -1 && column + dy > -1 && this.board[row + dx][column + dy] === opposite) {
        row += dx;
        column += dy;
    }
    row += dx;
    column +=dy;
    if ((row !== move.getRow() || column !== move.getColumn()) && row < 8 && column < 8 && row > -1 && column > -1 &&
        (row - move.getRow() !== dx || column - move.getColumn() !== dy) && this.board[row][column] === this.turn) {
        return true;
    } else {
        return false;
    }
};

ReversiBoad.prototype.toString = function () {
    let sb = "";
    for (let r = 1; r < 9; r++) {
        sb.concat(r.toString()).concat(" ");
        for (let c = 0; c < 8; c++) {
            sb.concat(this.board[r - 1][c]).concat(" ");
        }
        sb.concat("\n");
    }
    sb.concat("  A B C D E F G H");
    return sb;
};