package mateByRook;

import mateByRook.coordinates.FirstPosition;
import mateByRook.player.HumanPlayer;
import mateByRook.player.Winner;

public class MateByRook {
    private Game game;
    private Board board;

    public MateByRook(FirstPosition[] firstPositions) throws InputException {
        this.game = new Game(false, new Winner(), new HumanPlayer());
        this.board = new MateByRookBoard(firstPositions);
    }

    public int play() throws InputException {
        return game.play(board);
    }
}
