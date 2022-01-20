import jdistlib.rng.RandomEngine;
import java.util.*;

/**
 * Created by rebeccamancy on 30/05/2016.
 */
final public class Util {

    /**
     * Generates a random number from an exponential distribution
     * @param rate rate parameter of exponential discribution
     * @param randomEngine random engine
     * @return exponential random number (double)
     */
    public static double randExp(double rate, RandomEngine randomEngine) {
        return Math.log(1-randomEngine.nextDouble()) / (-rate);
    }

    /**
     * Generates a random double in an interval
     * @param rangeMin minimum of range
     * @param rangeMax maximum of range
     * @param randomEngine random engine
     * @return uniform random number (double)
     */
    public static double randInInterval(double rangeMin, double rangeMax, RandomEngine randomEngine) {
        return (rangeMin + (rangeMax - rangeMin) * randomEngine.nextDouble());
    }

    /**
     * Randomises a dog status list (i.e. list of dogs in each status in a population)
     * @param p Population
     * @param dogIDs IDs of dogs
     * @param rgen Random generator
     * @return Randomised ArrayList of dog status information
     */
    public static ArrayList<Epidemic.epiStatus> randDogStatusList (Population p, ArrayList<Integer> dogIDs, Random rgen) {

        ArrayList<Epidemic.epiStatus> popStatus = new ArrayList<Epidemic.epiStatus>(p.getTotalDogs());
        Iterator popIt = p.getEpiStatusHashmap().entrySet().iterator();
        while (popIt.hasNext()) {   // Loop over
            Map.Entry epiPair = (Map.Entry) popIt.next();
            for (int i=0; i<(Integer)epiPair.getValue(); i++) {
                popStatus.add((Epidemic.epiStatus) epiPair.getKey());
            }
        }

        Collections.shuffle(popStatus, rgen);

        ArrayList<Epidemic.epiStatus> statusesList = new ArrayList<Epidemic.epiStatus>();
        for (int id:dogIDs) {
            statusesList.add(popStatus.get(id));
        }
        return statusesList;
    }


    /**
     * Converts from array of doubles to csv format
     * @param arr array of doubles: double[]
     * @return String in csv format
     */
    public static String dblArr2csv (double[] arr) {
        String rtn = "";
        for (double entry:arr) {
            rtn = rtn + entry + ",";
        }
        return rtn;
    }

    /**
     * Converts from array of integer to csv format
     * @param arr array of int: int[]
     * @return String in csv format
     */
    public static String intArr2csv (int[] arr) {
        String rtn = "";
        for (int entry:arr) {
            rtn = rtn + entry + ",";
        }
        return rtn;
    }

    /**
     * Converts from ArrayList to array of doubles double[]
     * @param arrayList ArrayList<Double></Double> object
     * @return double[]
     */
    public static double[] ArrayListDouble2doubleArr (ArrayList<Double> arrayList) {
        double[] arrData = new double[arrayList.size()];
        int i = 0;
        for (Double entry : arrayList) {
            arrData[i] = entry;
            i++;
        }
        return arrData;
    }

    /**
     * A common method for all enums since they can't have another base class
     * @param <T> Enum type
     * @param c enum type. All enums must be all caps.
     * @param string case insensitive
     * @return corresponding enum, or null
     */
    public static <T extends Enum<T>> T getEnumFromString(Class<T> c, String string) {
        if( c != null && string != null ) {
            try {
                return Enum.valueOf(c, string.trim().toUpperCase());
            } catch(IllegalArgumentException ex) {
            }
        }
        return null;
    }
}
