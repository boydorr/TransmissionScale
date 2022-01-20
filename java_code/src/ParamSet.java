/**
 * Created by rebecca on 15/03/2017.
 */
public class ParamSet {
        private double ThShape, ThScale, Td;

    /**
     * Constructs a parameter set object consisting of handling and search time parameters
     * (from priors, at the level of a simulation run)
     * @param ThShape Shape parameter of handling time
     * @param ThScale Scale parameter of handling time
     * @param Td Search or "discovery" time parameter
     */
    public ParamSet(double ThShape, double ThScale, double Td) {
        this.ThShape = ThShape;
        this.ThScale = ThScale;
        this.Td = Td;
    }

    // Getters and toString
    public double getThShape() { return ThShape; }
    public double getTd() { return Td; }
    public double getThScale() { return ThScale; }
    public String toString() {
        return "ThShape = " + ThShape + ", ThScale = " + ThScale + ", Td = " + Td;
    }
}
