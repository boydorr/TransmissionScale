import jdistlib.rng.RandomEngine;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by rebeccamancy on 08/08/2016.
 * The SpatialUnit is an alternative format of aggregation (of Population objects) into e.g. villages.
 * This is no longer used in the simulation, and is included for back compatibility with csv output.
 */

public class SpatialUnit {

    private final ArrayList<Population> auPops = new ArrayList<>(); // Ordered according to which added
    private final HashMap<Epidemic.epiStatus, Integer> epiStatusHashmap; // Only used for initial storage, and not kept up to date!
    private final RandomEngine random;

    /**
     * Constructor of SpatialUnit object
     * @param name Name of the SpatialUnit
     * @param random random number generator
     */
    public SpatialUnit(String name, RandomEngine random) {

        epiStatusHashmap = new HashMap<>();
        for (Epidemic.epiStatus e : Epidemic.epiStatus.values()) {
            epiStatusHashmap.put(e, 0);
        }
        this.random = random;
    }

    /**
     * Adds a population to the epidemiological status hashmap at start
     * @param pop The population to add in
     */
    public void addPopulation (Population pop) {
        this.auPops.add(pop);
        epiStatusHashmap.put(Epidemic.epiStatus.S, epiStatusHashmap.get(Epidemic.epiStatus.S) + pop.getDogsS());
        epiStatusHashmap.put(Epidemic.epiStatus.E, epiStatusHashmap.get(Epidemic.epiStatus.E) + pop.getDogsE());
        epiStatusHashmap.put(Epidemic.epiStatus.V, epiStatusHashmap.get(Epidemic.epiStatus.V) + pop.getDogsV());
    }

}
