/**
 * Created by rebecca on 15/03/2017.
 */
public class EpiVariableSet {
        private double serialInterval, infectiousPeriod;

    /**
     * Constructs a set of epidemiological parameters, consisting of serial interval and infectious period
     * @param serialInterval
     * @param infectiousPeriod
     */
    public EpiVariableSet(double serialInterval, double infectiousPeriod) {
        this.serialInterval = serialInterval;
        this.infectiousPeriod = infectiousPeriod;
    }

    // Getters and toString methods
    public double getSerialInterval() { return serialInterval; }
    public double getInfectiousPeriod() { return infectiousPeriod; }

    public String toString() {
        return "serialInterval=" + serialInterval + ", infectiousPeriod=" + infectiousPeriod;
    }
}
