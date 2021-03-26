public class Ghost {

    private Direction direction;
    private String name;
    private int currentX;
    private int currentY;
    private final int NEXT_POS_ADDED_PIXELS = 1;

    // Needed when turn:
    private final int DISTANCE_FROM_BOUNDARY_X = 5; // when moving on Y - distance between ghost and boundary
    private final int DISTANCE_FROM_BOUNDARY_Y = 2; // when moving on X - distance between ghost and boundary

    /***
     * distance from boundary implements the distance of the ghost from boundary.
     * during the movment the ghost stays on a X\Y value. ghosts remain in the same
     * distance from each boundray.
     */

    public Ghost(Direction direction, String name, int currentX, int currentY) {

        this.direction = direction;
        this.name = name;
        this.currentX = currentX;
        this.currentY = currentY;
    }

    public Direction getDirection() {
        return this.direction;
    }

    public void setDirection(Direction newDirection) {
        this.direction = newDirection;

    }

    public void setCurrentX(int currentX) {

        this.currentX = currentX;
    }

    public void setCurrentY(int currentY) {

        this.currentY = currentY;
    }

    public int getCurrentX() {
        return currentX;
    }

    public int getCurrentY() {
        return currentY;
    }

    public boolean isYInRange(int Y) {

        return Y <= 200;
    }

    public boolean isXInRange(int X) {

        return X <= 320;
    }

    public void fixValues() {

        if (!isXInRange(this.currentX)) {
            this.currentX -= 320;
        }
        if (!isYInRange(this.currentY)) {
            this.currentY -= 200;
        }

    }

    // TODO: add func deteces whether changing pos gets to other siide of screen
    // TODO: turn depends on boudaries distance
    public void whenBlocked() {
        switch (this.direction) {

        case NORTH:

            if (Graphic.boundariesPos[this.currentX][this.currentY + NEXT_POS_ADDED_PIXELS] == 1) {

                // RIGHT TURN CHECK
                if (Graphic.boundariesPos[this.currentX + DISTANCE_FROM_BOUNDARY_X][this.currentY] != 1) {

                    setDirection(Direction.EAST);
                    setCurrentX(this.currentX + DISTANCE_FROM_BOUNDARY_X);

                }
                // LEFT TURN CHECK
                else if (Graphic.boundariesPos[this.currentX - DISTANCE_FROM_BOUNDARY_X][this.currentY] != 1) {

                    setDirection(Direction.WEST);
                    setCurrentX(this.currentX - DISTANCE_FROM_BOUNDARY_X);

                }
                // BACK TURN CHECK
                else if (Graphic.boundariesPos[this.currentX][this.currentY - NEXT_POS_ADDED_PIXELS] != 1) {

                    setDirection(Direction.SOUTH);
                    setCurrentY(this.currentY - NEXT_POS_ADDED_PIXELS);

                }
            } else {

                setCurrentY(currentY + NEXT_POS_ADDED_PIXELS);
            }

            fixValues();

            break;

        case SOUTH:

            if (Graphic.boundariesPos[this.currentX][this.currentY - NEXT_POS_ADDED_PIXELS] == 1) {

                // RIGHT TURN CHECK
                if (Graphic.boundariesPos[this.currentX + DISTANCE_FROM_BOUNDARY_X][this.currentY] != 1) {

                    setDirection(Direction.EAST);
                    setCurrentX(this.currentX + DISTANCE_FROM_BOUNDARY_X);

                }
                // LEFT TURN CHECK
                else if (Graphic.boundariesPos[this.currentX - DISTANCE_FROM_BOUNDARY_X][this.currentY] != 1) {

                    setDirection(Direction.WEST);
                    setCurrentX(this.currentX - DISTANCE_FROM_BOUNDARY_X);

                }
                // FORWARD TURN CHECK
                else if (Graphic.boundariesPos[this.currentX][this.currentY + NEXT_POS_ADDED_PIXELS] != 1) {

                    setDirection(Direction.NORTH);
                    setCurrentY(this.currentY + NEXT_POS_ADDED_PIXELS);

                }
            } else {

                setCurrentY(currentY + NEXT_POS_ADDED_PIXELS);
            }
            fixValues();

            break;

        case WEST:

            if (Graphic.boundariesPos[this.currentX + NEXT_POS_ADDED_PIXELS][this.currentY] == 1) {

                // FORWARD TURN CHECK
                if (Graphic.boundariesPos[this.currentX][this.currentY + DISTANCE_FROM_BOUNDARY_Y] != 1) {

                    setDirection(Direction.NORTH);
                    setCurrentY(this.currentY + NEXT_POS_ADDED_PIXELS);

                }
                // BACK TURN CHECK
                else if (Graphic.boundariesPos[this.currentX][this.currentY - DISTANCE_FROM_BOUNDARY_Y] != 1) {

                    setDirection(Direction.SOUTH);
                    setCurrentY(this.currentY - NEXT_POS_ADDED_PIXELS);

                }
                // LEFT TURN CHECK
                else if (Graphic.boundariesPos[this.currentX - NEXT_POS_ADDED_PIXELS][this.currentY] != 1) {

                    setDirection(Direction.EAST);
                    setCurrentX(this.currentX - DISTANCE_FROM_BOUNDARY_X);

                }
            } else {
                setCurrentX(currentX + NEXT_POS_ADDED_PIXELS);
            }
            fixValues();

            break;

        case EAST:

            if (Graphic.boundariesPos[this.currentX - NEXT_POS_ADDED_PIXELS][this.currentY] == 1) {

                // FORWARD TURN CHECK
                if (Graphic.boundariesPos[this.currentX][this.currentY + DISTANCE_FROM_BOUNDARY_X] != 1) {

                    setDirection(Direction.NORTH);
                    setCurrentY(this.currentY + NEXT_POS_ADDED_PIXELS);

                }
                // BACK TURN CHECK
                else if (Graphic.boundariesPos[this.currentX][this.currentY - DISTANCE_FROM_BOUNDARY_X] != 1) {

                    setDirection(Direction.SOUTH);
                    setCurrentY(this.currentY - NEXT_POS_ADDED_PIXELS);

                }
                // LEFT TURN CHECK
                else if (Graphic.boundariesPos[this.currentX + NEXT_POS_ADDED_PIXELS][this.currentY] != 1) {

                    setDirection(Direction.WEST);
                    setCurrentX(this.currentX - DISTANCE_FROM_BOUNDARY_X);

                }
            } else {

                setCurrentX(currentX - NEXT_POS_ADDED_PIXELS);
            }
            fixValues();

            break;
        }
    }

    @Override
    public String toString() {

        return "Ghost: " + name + "moves on the " + getDirection() + "\n current X: " + getCurrentX() + "\n current y: "
                + getCurrentY();
    }

}