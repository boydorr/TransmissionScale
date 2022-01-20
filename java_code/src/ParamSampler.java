import jdistlib.rng.RandomEngine;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by rebecca on 15/03/2017.
 * Used for sampling from list of priors
 */
public class ParamSampler {
    ArrayList<ParamSet> sampledParams = new ArrayList<ParamSet>();
    Boolean sampledPriors;
    String folder, fileName;

    /**
     * Constructs a parameter sampler object
     * @param folder
     * @param fileName
     * @param sampledPriors
     */
    public ParamSampler(String folder, String fileName, Boolean sampledPriors) {
        this.folder = folder;
        this.sampledPriors = sampledPriors;
        this.fileName = fileName;
    }

    /**
     * Generates an ArrayList of parameter sets from a file
     * @param nParamsets Number of parameter sets required
     * @param random random number generator
     * @param experimentID Experiment/run ID
     * @return
     */
    public ArrayList<ParamSet> sampleParams(int nParamsets, RandomEngine random, String experimentID) {

        if (sampledPriors) { // Draw from a set of discrete values
            ArrayList<ParamSet> samplingParams = SamplingParamsReader.readCSVFile(folder, fileName);

            // If we don't have the same number as the number of runs to do, then randomly sample from these
            if (samplingParams.size() != nParamsets) {
                for (int n = 0; n < nParamsets; n++) {
                    sampledParams.add(samplingParams.get(random.nextInt(samplingParams.size())));
                }
            } else {
                for (int n = 0; n < nParamsets; n++) {
                    sampledParams.add(samplingParams.get(n)); // Straight copy
                }
            }
            System.out.println("Number of sampled parameters: " + sampledParams.size() + " number of paramSets " + nParamsets);
        } else { // Draw from uniform priors, the bounds of which are provided
            HashMap<String, Priors> priorsHashMap = PriorsReader.readCSVFile(folder, fileName);
            Priors cPriors = priorsHashMap.get(experimentID);
            for (int n = 0; n < nParamsets; n++) {
                System.err.println("Not implemented in ParamSampler > sampleParams");
                System.exit(1);
                // sampledParams.add(cPriors.simulateParamset(random));
            }
        }
        return sampledParams;
    }

}
