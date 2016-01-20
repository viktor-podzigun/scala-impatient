
public class Chapter05Car {

    private final String manufacturer;
    private final String modelName;
    private final int modelYear;
    private final String licensePlate;

    public Chapter05Car(final String manufacturer, final String modelName) {
        this(manufacturer, modelName, -1);
    }

    public Chapter05Car(final String manufacturer, final String modelName, final int modelYear) {
        this(manufacturer, modelName, modelYear, "");
    }

    public Chapter05Car(final String manufacturer,
            final String modelName,
            final String licensePlate) {

        this(manufacturer, modelName, -1, licensePlate);
    }

    public Chapter05Car(final String manufacturer,
            final String modelName,
            final int modelYear,
            final String licensePlate) {

        this.manufacturer = manufacturer;
        this.modelName = modelName;
        this.modelYear = modelYear;
        this.licensePlate = licensePlate;
    }
}
