import jdistlib.rng.RandomEngine;

import java.util.ArrayList;

/**
 * Created by rebecca on 15/03/2017.
 */
public class EpiVariableSampler {
    ArrayList<EpiVariableSet> sampleFrom = new ArrayList<EpiVariableSet>();
    String folder, fileName;
    Boolean isSampled;

    /**
     * Constructs a sampler for epidemiological parameters by reading in from a csv file
     * @param folder
     * @param fileName
     * @param isSampled
     */
    public EpiVariableSampler(String folder, String fileName, Boolean isSampled) {
        this.folder = folder;
        this.isSampled = isSampled;
        this.fileName = fileName;
        sampleFrom = SamplingEpiVariableReader.readCSVFile(folder, fileName);
    }

    /**
     * Draws a set of epidemiological parameters from the ArrayList of parameters
     * @param random
     * @return a parameter set
     */
    public EpiVariableSet sampleEpiVariables(RandomEngine random) {

        EpiVariableSet epiVariableSet = null;

        if (isSampled) { // Draw from a pre-computed set of values

            epiVariableSet = sampleFrom.get(random.nextInt(sampleFrom.size())); // Straight copy from the set we're drawing from
            return epiVariableSet;

        } else { // Draw from uniform priors, the bounds of which are provided

            System.out.println("NOT IMPLEMENTED"); System.exit(0);

        }

        return epiVariableSet;
    }

}
