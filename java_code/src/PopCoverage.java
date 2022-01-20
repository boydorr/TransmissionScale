/**
 * Used in computing timeseries of vaccination coverage at the level of the whole mp combined into a single mixedPop
 */
public class PopCoverage implements Comparable<PopCoverage> {
    private double time;
    private int dogsS, dogsE, dogsV, N;
    private double K, coverage;
    private static final String COMMA_DELIMITER = ",";

    /**
     * Constructs a population coverage object
     * @param time time at which coverage is censused
     * @param p a population (within the metapopulation
     */
    public PopCoverage(double time, Population p) {
        this.time = time;
        this.dogsS = p.getDogsS();
        this.dogsE = p.getDogsE();
        this.dogsV = p.getDogsV();
        this.N = p.getTotalDogs();
        this.K = p.getK();
        this.coverage = (double)dogsV/(double)N;
    }

    public int compareTo(final PopCoverage other ) {if (other == null) {
        return 1;
    }
        final int comp = Double.compare(this.time, other.time);
        return comp;
    }

    public String toString() {
        return "Time=" + time + " " + N;
    }

    /**
     * Creates a header row for CSV file
     * @return header of csv file
     */
    public static String toStringCsvHeader() {
        return "time, N, S, E, V, K, coverage";
    }

    /**
     * Converts case information to a String in csv format
     * @return one row of the csv file
     */
    public String toStringCsv() {
        String returnString =   time + COMMA_DELIMITER +
                N + COMMA_DELIMITER +
                dogsS + COMMA_DELIMITER +
                dogsE + COMMA_DELIMITER +
                dogsV + COMMA_DELIMITER +
                K + COMMA_DELIMITER +
                coverage;
        return returnString;
    }

}

