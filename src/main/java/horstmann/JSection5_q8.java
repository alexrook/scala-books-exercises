package horstmann;

import static java.lang.System.out;

public class JSection5_q8 {

    public static class Car {

        private String manufacturer, model, regNum;
        private int year = -1;

        public Car(String manufacturer, String model, int year, String regNum) {
            this.manufacturer = manufacturer;
            this.model = model;
            this.year = year;
            setRegNum(regNum);
        }

        public Car(String manufacturer, String model, String regNum) {
            this(manufacturer, model, -1, regNum);
        }

        public Car(String manufacturer, String model, int year) {
            this(manufacturer, model, year, "");
        }

        public Car(String manufacturer, String model) {
            this(manufacturer, model, -1, "");
        }

        public String getManufacturer() {
            return manufacturer;
        }

        public String getModel() {
            return model;
        }

        public int getYear() {
            return year;
        }

        public String getRegNum() {
            return regNum;
        }

        public void setRegNum(String regNum) {
            this.regNum = regNum;
        }

        @Override
        public String toString() {
            //s"$manufacturer:$model, $year, $regNum"
            return getManufacturer() + ":" + getModel() + ", " + getYear() + "," + getRegNum();
        }
    }

    public static void main(String[] args) {

        out.println("Java JSection5_q8.Car's:");
        Car blackMoon = new Car("BigFactory",
                "BlackMoon", 1977, "#1");
        out.println(blackMoon);
        blackMoon.setRegNum("1");
        out.println(blackMoon);

        Car cabby = new Car("GarageInc", "Yellow Cab");
        out.println(cabby);
        cabby.setRegNum("#77799");
        out.println(cabby);
    }
}
