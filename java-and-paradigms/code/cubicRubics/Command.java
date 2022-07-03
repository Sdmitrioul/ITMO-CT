package cubicRubics;

public class Command {
    private Edge edge;
    private boolean direction;

    public void setEdge(Edge edge) {
        this.edge = edge;
    }

    public Command(Edge edge) {
        this.edge = edge;
        direction = true;
    }

    public void setDirection(boolean direction) {
        this.direction = direction;
    }

    public Edge getEdge() {
        return edge;
    }

    public boolean isDirection() {
        return direction;
    }
}
