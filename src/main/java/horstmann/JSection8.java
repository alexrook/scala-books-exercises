package horstmann;


public class JSection8 {

    public static class L82JBase {

        private int x, y;

        public L82JBase(int x) {
            setX(x);
        }

        public L82JBase(int x, int y) {
            this.x = x;
            this.y = y;
        }

        public void setX(int x) {
            this.x = x;
        }

        public int getY() {
            return y;
        }

        public int getX() {
            return x;
        }
    }

    public static class L82JSub extends L82JBase {
        public L82JSub() {
            super(0, 0);
        }
    }

    public static void main(String[] args) {
        L82JSub l82JSub1 = new L82JSub();

    }

}
