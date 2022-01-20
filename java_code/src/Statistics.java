import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

/**
 * Created by rebecca on 07/02/2017.
 * Utilities used to compute various statistics of interest; used by ABCOutcomes and Metapopulation.StandardisationParameters
 */
public class Statistics
{
    private final double[] data;
    private final int size;

    /**
     * Constructor based directly on array of doubles
     * @param data array of doubles
     */
    public Statistics(double[] data)
    {
        this.data = data;
        size = data.length;
    }

    /**
     * Constructor based on a TreeSet. Converts to arrData.
     * @param arrayList
     */
    public Statistics(ArrayList<Double> arrayList)
    {
        double[] arrData = new double[arrayList.size()];
        int i = 0;
        for (Double entry : arrayList) {
            arrData[i] = entry;
            i++;
        }
        this.data = arrData;
        size = arrayList.size();
    }

    /**
     * Constructor based on a TreeSet. Converts to arrData.
     * @param map
     */
    public Statistics(HashMap<Integer, Double> map)
    {
        double[] arrData = new double[map.size()];
        int i = 0;
        for (Double entry : map.values()) {
            arrData[i] = entry;
            i++;
        }
        this.data = arrData;
        size = this.data.length;
     }

    /**
     * Get number of items in the array
     * @return
     */
    private int getSize() { return size; }

    /**
     * Computes the mean
     * @return
     */
    double getMean()
    {
        double sum = 0.0;
        for(double a : data) {
            sum = sum + a;
        }
        return sum/getSize();
    }

    /**
     * Finds the maximum value
     * @return
     */
    double getMax()
    {
        double max = 0.0;
        for(double a : data) {
            if (a > max) max = a;
        }
        return max;
    }

    /**
     * Computes the variance
     * @return
     */
    double getVariance()
    {
        double mean = getMean();
        double temp = 0;
        for (double a :data) {
            temp = temp + (a-mean)*(a-mean);
        }
        return temp/size;
    }

    /**
     * Computes the standard deviation
     * @return
     */
    double getStdDev()
    {
        return Math.sqrt(getVariance());
    }

    /**
     * Computes the median
     * @return
     */
    public double getMedian()
    {
        Arrays.sort(data);

        if (data.length % 2 == 0)
        {
            return (data[(data.length / 2) - 1] + data[data.length / 2]) / 2.0;
        }
        return data[data.length / 2];
    }
}
