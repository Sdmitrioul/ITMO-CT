package ticTacToe;

public interface Board {
    Result makeMove(Move move);
    Cell getCell();
    Position getPosition();
    Settings getSettings();
}
