import jdistlib.Gamma;
import jdistlib.LogNormal;
import jdistlib.rng.RandomEngine;

/**
 * Created by rebeccamancy on 03/05/2015.
 */
public class EpidemiologicalModel {
    private double transmissionProb;            // Probability of transmission given a bite
    RandomEngine random;
    String modelType;
    EpiVariableSampler epiVariableSampler;

    /**
     * Constructs and epidemiological model
     * @param folderEpiVariables
     * @param fEpiVariables
     * @param modelType No longer really required, but tells us if incubation and infectious periods are sampled from the data
     * @param transmissionProb Probability of transmission given a bit
     * @param random Random number generator
     */
    public EpidemiologicalModel(String folderEpiVariables, String fEpiVariables, String modelType, double transmissionProb, RandomEngine random) {
        this.random = random;

        // Set up a model in which incubation and infectious periods are drawn from the data
        this.modelType = modelType;
        System.out.println("Constructing an EpidemiologicalModel of type modelType: " + modelType + " using data from " + folderEpiVariables + fEpiVariables);
        Boolean isSampled = Boolean.TRUE;
        epiVariableSampler = new EpiVariableSampler(folderEpiVariables, fEpiVariables, isSampled);

        // Set probability of transmission given bite
        this.transmissionProb = transmissionProb;

    }

    /**
     * Returns a parameter set drawn from the epidemic variable values read in
     * @return
     */
    public EpiVariableSet randEpiVariableSet() {
        EpiVariableSet ev = epiVariableSampler.sampleEpiVariables(random);
        /// System.out.println("Epidemic variables: " + ev.toString());
        return ev;
    }


    /**
     * Returns whether transmission occurs (based on transmission probability)
     * @return
     */
    public Boolean isTransmissionEffective() { return random.nextDouble() < transmissionProb; }
}
