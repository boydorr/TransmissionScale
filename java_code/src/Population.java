import com.vividsolutions.jts.geom.Geometry;
import java.util.HashMap;
import java.util.Iterator;
import java.util.TreeSet;

/**
 * Created by rebeccamancy on 10/08/2015.
 * Each Population holds the number of dogs in each
 * disease state, as well as carrying capacity information, and event rates.
 * A Metapopulation has an ArrayList of Population objects.
 */
public class Population implements Comparable<Population>{

    private final HashMap<Epidemic.epiStatus, Integer> epiStatusHashmap = new HashMap<Epidemic.epiStatus, Integer>();
    private double initialK;
    private final HashMap<Double, Double> mapDaysK = new HashMap<Double, Double>();
    private double K; // Current carrying capacity
    private int popID;
    private String name;
    private Geometry geo;
    private double eventRate, birthRate, deathRate;

    /**
     * Constructor; assumes all dogs initially susceptible
     * @param totalDogs Total number of dogs
     * @param name Name of population
     * @param popID ID of population
     */
    public Population(int totalDogs, String name, int popID) {
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, totalDogs);
        this.initialK = totalDogs;
        this.K = totalDogs; // Set carrying capacity to baseline dog counts
        this.popID = popID;
        this.name = name;
    }

    /**
     * Population constructor with time-consistent carrying capacities
     * @param popID population identifier
     * @param auName Name of SpatialUnit
     * @param updateKDays Days on which to update carrying capacity
     * @param popSizes Population sizes when updating is to take place
     */
    public Population(int popID, String auName, TreeSet<Double> updateKDays, double[] popSizes) {
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        // Add population sizes over time to mapDaysK
        Iterator<Double> itr = updateKDays.iterator();
        int i = 0;
        while (itr.hasNext()) {
            this.mapDaysK.put(itr.next(), popSizes[i]);
            i++;
        }

        // Initial population values
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, (int) Math.round(popSizes[0]));
        this.initialK = popSizes[0];
        this.K = popSizes[0]; // Set carrying capacity to baseline dog counts
        this.popID = popID;
        this.name = auName;
    }

    /**
     * Constructor that is used to create a copy of a Population object (for EpidemicOutcomes - gets
     * around deep-shallow copying problems)
     * @param pop Population to be copied
     */
    public Population(Population pop) {
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        this.initialK = pop.getInitialK();
        this.K = pop.getK();
        this.popID = pop.getPopID();
        this.name = pop.getName();
        this.geo = pop.getGeo();
        this.eventRate = pop.getEventRate();
        this.birthRate = pop.getBirthRate();
        this.deathRate = pop.getDeathRate();
    }

    /**
     * Constructor for a dog population that is used when we combine populations into a Metapopulation (mp)
     * @param totalDogs Total number of dogs
     */
    public Population(int totalDogs) {
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, totalDogs);
        this.K = totalDogs; // Set carrying capacity to baseline dog counts
    }

    /**
     * Constructor used when combining two populations, each associated with a grid cell
     * @param dogsS Number of susceptible dogs
     * @param dogsE Number of exposed dogs
     * @param dogsV Number of vaccinated dogs
     * @param K Carrying capacity of grid cell
     * @param eventRate event rate in grid cell
     * @param birthRate birth rate in grid cell
     * @param deathRate death rate of grid cell
     */
    private Population(int dogsS, int dogsE, int dogsV, double K, double eventRate, double birthRate, double deathRate) {
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, dogsS);
        this.epiStatusHashmap.put(Epidemic.epiStatus.E, dogsE);
        this.epiStatusHashmap.put(Epidemic.epiStatus.V, dogsV);
        this.K = K;
        this.eventRate = eventRate;
        this.birthRate = birthRate;
        this.deathRate = deathRate;
    }

    /**
     * Resets a Population object to the starting values, all with status S
     */
    public void reset() {
        this.epiStatusHashmap.clear();
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, (int) Math.round(this.initialK));
    }

    /**
     * Returns header row for a csv-type output of Population object
     * @return
     */
    public String getPopHeader() {
        String popHeader = "popID, name, K, S, V, E, totalDogs, coverage";
        return popHeader;
    }

    /**
     * Returns the hashmap containing the disease states and counts
     * @return
     */
    public HashMap<Epidemic.epiStatus, Integer> getEpiStatusHashmap() {
        return epiStatusHashmap;
    }

    /**
     * Sets K (carrying capacity), and updates demographic parameters accordingly
     * @param K
     * @param dm
     */
    public void setK(double K, DemographicModel dm) {
        this.K = K;
        this.eventRate = dm.eventRate(this);
        this.deathRate = dm.computeDeathRatePop(this);
        this.birthRate = dm.computeAugmentationRatePop(this);
    }

    /**
     * Returns the HashMap containing days and carrying capacities for this population
     * @return
     */
    public HashMap<Double, Double> getMapDaysK() { return mapDaysK; }

    /**
     * Returns the total number of dogs in the Population
     * @return
     */
    public int getTotalDogs() {
        return (getDogsS() + getDogsE() + getDogsV());
    }


    // Get and set methods
    public void setGeometry (Geometry geo) {
        this.geo = geo;
    }

    public Geometry getGeo() {
        return this.geo;
    }

    public double getInitialK() { return initialK; }

    public String getName() {
        return this.name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setEventRate(double eventRate) {
        this.eventRate = eventRate;
    }

    public void setBirthRate(double birthRate) { this.birthRate = birthRate; }

    public void setDeathRate(double deathRate) { this.deathRate = deathRate; }

    public double getEventRate() {
        return eventRate;
    }

    public double getBirthRate() { return birthRate; }

    public double getDeathRate() { return deathRate; }

    public int getDogsS() {
        return this.epiStatusHashmap.get(Epidemic.epiStatus.S);
    }

    public int getDogsE() {
        if (epiStatusHashmap.containsKey(Epidemic.epiStatus.E)) {
            return this.epiStatusHashmap.get(Epidemic.epiStatus.E);
        } else {
            return 0;
        }
    }

    public int getDogsV() {
        if (epiStatusHashmap.containsKey(Epidemic.epiStatus.V)) {
            return this.epiStatusHashmap.get(Epidemic.epiStatus.V);
        } else {
            return 0;
        }
    }

    public double getK() {
        return K;
    }

    public int getPopID() { return popID; }

    public void setDogsS(int dogsS) {
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, dogsS);
    }

    /**
     * Adds a susceptible individual to the population
     */
    public void doBirth() {
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, this.epiStatusHashmap.get(Epidemic.epiStatus.S)+1);
    }

    /**
     * S or E or V -> dies   (Death from any of the compartments)
     * @param dogStatus
     */
    public void doDeath(Epidemic.epiStatus dogStatus) {
        switch (dogStatus) {
            case S:
                this.epiStatusHashmap.put(Epidemic.epiStatus.S, this.epiStatusHashmap.get(Epidemic.epiStatus.S)-1);
                break;
            case E: // This is called when an infectious dog dies too - see doInfectiousDeath below
                this.epiStatusHashmap.put(Epidemic.epiStatus.E, this.epiStatusHashmap.get(Epidemic.epiStatus.E)-1);
                break;
            case V:
                this.epiStatusHashmap.put(Epidemic.epiStatus.V, this.epiStatusHashmap.get(Epidemic.epiStatus.V)-1);
                break;
        }
    }

    /**
     * S -> E
     */
    public void doInfection() {
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, this.epiStatusHashmap.get(Epidemic.epiStatus.S)-1);
        if (epiStatusHashmap.containsKey(Epidemic.epiStatus.E)) {
            this.epiStatusHashmap.put(Epidemic.epiStatus.E, this.epiStatusHashmap.get(Epidemic.epiStatus.E)+1);
        } else {   // first E in this population
            epiStatusHashmap.put(Epidemic.epiStatus.E, 1);
        }

    }

    /**
     * Vaccination event - susceptible dogs become vaccinated (bulk process)
     * @param numDogs
     */
    public void doVaccination(int numDogs) {
        this.epiStatusHashmap.put(Epidemic.epiStatus.S, this.epiStatusHashmap.get(Epidemic.epiStatus.S)-numDogs);
        this.epiStatusHashmap.put(Epidemic.epiStatus.V, this.epiStatusHashmap.get(Epidemic.epiStatus.V)+numDogs);
    }

    /**
     * Combine populations (e.g. to construct equivalent fully-mixed population of several populations)
     * @param p2   The population to add in
     * @return
     */
    public Population combinePops (Population p2) {
        return new Population(this.getDogsS()+p2.getDogsS(), this.getDogsE()+p2.getDogsE(), this.getDogsV()+p2.getDogsV(), this.getK()+p2.getK(), this.getEventRate()+p2.getEventRate(), this.getBirthRate() + p2.getBirthRate(), this.getDeathRate() + p2.getDeathRate() );
    }

    /**
     * Returns a printable summary of the Population
     * @return
     */
    public String toString() {
        return "Population_ID=" + getPopID() + " Name=" + getName() + " N=" + getTotalDogs() + " S=" + getDogsS() + " E=" + getDogsE() + " V=" + getDogsV() + " K=" + getK() +
                " eventRate=" + getEventRate() + " birthRatePop=" + getBirthRate() + " computeDeathRatePop=" + getDeathRate() + " coverage=" + ((double)getDogsV() / (double)getTotalDogs());
    }

    /**
     * Returns the Population attributes in csv format
     * @return
     */
    public String toCSVString() {
        return getPopID() + "," + getName() + "," + getK() + "," + getDogsS() + "," + getDogsV() + "," + getDogsE() + "," + getTotalDogs() + "," + ((double)getDogsV() / (double)getTotalDogs());
    }

    /**
     * Returns header for the Population attributes in csv format
     * @return
     */
    public static String populationCSVHeader() {
        return "numMonths, popID, popName, S, V, E, totalDogs, coverage";
    }

    /**
     * compareTo allows sorting order for list of populations for the list of populations TreeSet (sort by popID)
     * @param other
     * @return
     */
    public int compareTo(final Population other) {
        if (other == null) {
            return 1;
        }
        final int comp = this.name.compareTo(other.name);
        if (comp == 0) {
            return this.popID - other.popID;
        } else {
            return comp;
        }
    }
}
