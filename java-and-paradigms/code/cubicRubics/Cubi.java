package cubicRubics;

import java.util.Map;

public class Cubi {
    private Colour[] left;
    private Colour[] right;
    private Colour[] back;
    private Colour[] front;
    private Colour[] up;
    private Colour[] down;

    private static final Map<Colour, Character> COLOUR_CHARACTER_MAP = Map.of(
            Colour.B, 'B',
            Colour.W, 'W',
            Colour.G, 'G',
            Colour.O, 'O',
            Colour.Y, 'Y',
            Colour.R, 'R'
    );

    private static final Map<Integer, Integer> ROTATION_EDGE = Map.of(
            2, 0,
            5, 1,
            8, 2,
            1, 3,
            4, 4,
            7, 5,
            0, 6,
            3, 7,
            6, 8

    );

    private static final Map<Integer, Integer> REVERS_ROTATION_EDGE = Map.of(
            6, 0,
            3, 1,
            0, 2,
            7, 3,
            4, 4,
            1, 5,
            8, 6,
            5, 7,
            2, 8

    );

    public Cubi() {
        left = new Colour[9];
        right = new Colour[9];
        back = new Colour[9];
        front = new Colour[9];
        up = new Colour[9];
        down = new Colour[9];
        for (int i = 0; i < 9; i++) {
            up[i] = Colour.W;
            left[i] = Colour.R;
            front[i] = Colour.B;
            right[i] = Colour.O;
            back[i] = Colour.G;
            down[i] = Colour.Y;
        }
    }

    public void rotation(Command command) {
        if (command.isDirection()) {
            switch (command.getEdge()) {
                case U:
                    Colour[] tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = up[ROTATION_EDGE.get(i)];
                    }
                    up = tmp;
                    Colour[] tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = front[i];
                    }
                    for (int i = 0; i < 3; i++) {
                        front[i] = right[i];
                        right[i] = back[i];
                        back[i] = left[i];
                        left[i] = tm[i];
                    }
                    break;
                case L:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = left[ROTATION_EDGE.get(i)];
                    }
                    left = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i * 3];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i * 3] = back[8 - i * 3];
                        back[8 - i * 3] = down[i * 3];
                        down[i * 3] = front[i * 3];
                        front[i * 3] = tm[i];
                    }
                    break;
                case F:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = front[ROTATION_EDGE.get(i)];
                    }
                    front = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[6 + i];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[6 + i] = left[8 - 3 * i];
                        left[8 - 3 * i] = down[2 - i];
                        down[2 - i] = right[3 * i];
                        right[3 * i] = tm[i];
                    }
                    break;
                case R:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = right[ROTATION_EDGE.get(i)];
                    }
                    right = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i * 3 + 2];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i * 3 + 2] = front[i * 3 + 2];
                        front[i * 3 + 2] = down[i * 3 + 2];
                        down[i * 3 + 2] = back[6 - i * 3];
                        back[6 - i * 3] = tm[i];
                    }
                    break;
                case B:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = back[ROTATION_EDGE.get(i)];
                    }
                    back = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i] = right[2 + i * 3];
                        right[2 + i * 3] = down[8 - i];
                        down[8 - i] = left[6 - 3 * i];
                        left[6 - 3 * i] = tm[i];
                    }
                    break;
                case D:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = down[ROTATION_EDGE.get(i)];
                    }
                    down = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = front[i + 6];
                    }
                    for (int i = 6; i < 9; i++) {
                        front[i] = left[i];
                        left[i] = back[i];
                        back[i] = right[i];
                        right[i] = tm[i - 6];
                    }
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + command.getEdge());
            }
        } else if (!command.isDirection()) {
            switch (command.getEdge()) {
                case U:
                    Colour[] tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = up[REVERS_ROTATION_EDGE.get(i)];
                    }
                    up = tmp;
                    Colour[] tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = front[i];
                    }
                    for (int i = 0; i < 3; i++) {
                        front[i] = left[i];
                        left[i] = back[i];
                        back[i] = right[i];
                        right[i] = tm[i];
                    }
                    break;
                case L:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = left[REVERS_ROTATION_EDGE.get(i)];
                    }
                    left = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i * 3];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i * 3] = front[i * 3];
                        front[i * 3] = down[i * 3];
                        down[i * 3] = back[8 - i * 3];
                        back[8 - i * 3] = tm[i];
                    }
                    break;
                case F:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = front[REVERS_ROTATION_EDGE.get(i)];
                    }
                    front = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[6 + i];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[6 + i] = right[3 * i];
                        right[3 * i] = down[2 - i];
                        down[2 - i] = left[8 - 3 * i];
                        left[8 - 3 * i] = tm[i];
                    }
                    break;
                case R:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = right[REVERS_ROTATION_EDGE.get(i)];
                    }
                    right = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i * 3 + 2];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i * 3 + 2] = back[6 - i * 3];
                        back[6 - i * 3] = down[i * 3 + 2];
                        down[i * 3 + 2] = front[i * 3 + 2];
                        front[i * 3 + 2] = tm[i];
                    }
                    break;
                case B:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = back[REVERS_ROTATION_EDGE.get(i)];
                    }
                    back = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = up[i];
                    }
                    for (int i = 0; i < 3; i++) {
                        up[i] = left[6 - i * 3];
                        left[6 - i * 3] = down[8 - i];
                        down[8 - i] = right[2 + 3 * i];
                        right[2 + 3 * i] = tm[i];
                    }
                    break;
                case D:
                    tmp = new Colour[9];
                    for (int i = 0; i < 9; i++) {
                        tmp[i] = down[REVERS_ROTATION_EDGE.get(i)];
                    }
                    down = tmp;
                    tm = new Colour[3];
                    for (int i = 0; i < 3; i++) {
                        tm[i] = front[i + 6];
                    }
                    for (int i = 6; i < 9; i++) {
                        front[i] = right[i];
                        right[i] = back[i];
                        back[i] = left[i];
                        left[i] = tm[i - 6];
                    }
                    break;
                default:
                    throw new IllegalStateException("Unexpected value: " + command.getEdge());
            }
        }
    }

    public String toString() {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 3; i++) {
            sb.append("   ");
            int k = i * 3;
            sb.append(COLOUR_CHARACTER_MAP.get(up[k])).append(COLOUR_CHARACTER_MAP.get(up[k + 1])).append(COLOUR_CHARACTER_MAP.get(up[k + 2]));
            sb.append("\n");
        }
        for (int i = 0; i < 3; i++) {
            int k = i * 3;
            sb.append(COLOUR_CHARACTER_MAP.get(left[k])).append(COLOUR_CHARACTER_MAP.get(left[k + 1])).append(COLOUR_CHARACTER_MAP.get(left[k + 2]));
            sb.append(COLOUR_CHARACTER_MAP.get(front[k])).append(COLOUR_CHARACTER_MAP.get(front[k + 1])).append(COLOUR_CHARACTER_MAP.get(front[k + 2]));
            sb.append(COLOUR_CHARACTER_MAP.get(right[k])).append(COLOUR_CHARACTER_MAP.get(right[k + 1])).append(COLOUR_CHARACTER_MAP.get(right[k + 2]));
            sb.append(COLOUR_CHARACTER_MAP.get(back[k])).append(COLOUR_CHARACTER_MAP.get(back[k + 1])).append(COLOUR_CHARACTER_MAP.get(back[k + 2]));
            sb.append("\n");
        }
        for (int i = 0; i < 3; i++) {
            sb.append("   ");
            int k = i * 3;
            sb.append(COLOUR_CHARACTER_MAP.get(down[k])).append(COLOUR_CHARACTER_MAP.get(down[k + 1])).append(COLOUR_CHARACTER_MAP.get(down[k + 2]));
            if (i < 2) {
                sb.append("\n");
            }
        }
        return sb.toString();
    }
}
