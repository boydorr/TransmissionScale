/**
 * Created by rebeccamancy on 30/08/2015.
 * No longer used because we now read priors in from a file; maintained for backward compatibility
 */
public class Priors  {
    private static final String COMMA_DELIMITER = ", ";
    private String model, spatialScale;
    private double ThMeanMin, ThMeanMax, ThSDMin, ThSDMax, TdMeanMin, TdMeanMax, incursionRateMin, incursionRateMax;

    /**
     * Constructs a Priors object
     * @param model
     * @param spatialScale
     * @param ThMeanMin
     * @param ThMeanMax
     * @param ThSDMin
     * @param ThSDMax
     * @param TdMeanMin
     * @param TdMeanMax
     * @param incursionRateMin
     * @param incursionRateMax
     */
    public Priors(String model, String spatialScale, double ThMeanMin, double ThMeanMax, double ThSDMin, double ThSDMax, double TdMeanMin, double TdMeanMax, double incursionRateMin, double incursionRateMax) {
        this.model = model;
        this.spatialScale = spatialScale;
        this.ThMeanMin = ThMeanMin;
        this.ThMeanMax = ThMeanMax;
        this.ThSDMin = ThSDMin;
        this.ThSDMax = ThSDMax;
        this.TdMeanMin = TdMeanMin;
        this.TdMeanMax = TdMeanMax;
        this.incursionRateMin = incursionRateMin;
        this.incursionRateMax = incursionRateMax;
    }

    // Getters, setters and toString
    public String getSpatialScale() {
        return spatialScale;
    }

    public String getModel() {
        return model;
    }

    public double getThMeanMin() {
        return ThMeanMin;
    }

    public double getThMeanMax() {
        return ThMeanMax;
    }

    public double getThSDMin() {
        return ThSDMin;
    }

    public double getThSDMax() {
        return ThSDMax;
    }

    public double getTdMeanMin() {
        return TdMeanMin;
    }

    public double getTdMeanMax() {
        return TdMeanMax;
    }

    public double getIncursionRateMin() {
        return incursionRateMin;
    }

    public double getIncursionRateMax() {
        return incursionRateMax;
    }

    public String toString() {
        return "model:" + getModel() + COMMA_DELIMITER + " spatialScale:" + getSpatialScale() + COMMA_DELIMITER + " ThMeanMin:" + getThMeanMin() + COMMA_DELIMITER + " ThMeanMax:" + getThMeanMax() + COMMA_DELIMITER +
                " ThSDMin:" + getThSDMin() + COMMA_DELIMITER + " ThSDMax:" + getThSDMax() + COMMA_DELIMITER +
                " TdMeanMin:" + getTdMeanMin() + COMMA_DELIMITER + " TdMeanMax:" + getTdMeanMax() + COMMA_DELIMITER +
                " incursionRateMin:" + getIncursionRateMin() + COMMA_DELIMITER + " incursionRateMax:" + getIncursionRateMax();
    }
}
