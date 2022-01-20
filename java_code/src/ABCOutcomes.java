/**
 * Created by rebeccamancy on 05/02/2017.
 */


import jdistlib.Poisson;
import jdistlib.rng.RandomEngine;
import jsc.independentsamples.MannWhitneyTest;

import java.util.*;
import static jsc.tests.H1.NOT_EQUAL;

public class ABCOutcomes {

    private Boolean stoppedEarly;
    private String stoppingReason;
    private ArrayList<Case> caseList;
    private EpidemicProperties ep;
    private Poisson poisson;
    private double[] monthBreaks, caseCountBreaks, densityBreaks;
    private double TdMean, ThMean, ThShape, ThScale, incursionRate, meanDD, sdDD, R0 = 0, meanStdDensity500, meanStdDensity1000, meanStdDensity4000, maxDD = 0; int numDetectableDogs = 0, numNonContributors = 0, numSuperspreaders = 0;
    private double tsMW = 0, density1000MW = 0, positiveDensity1000MW = 0, densityDependence = -1;
    private double[] monthlyTally; // Each entry represents the number of cases in that month (double for convenience wrt Mann-Whitney test)
    private double[] monthlyHistogram;  // Each entry = num months with the given num of cases
    private MonthPops[] monthPops500, monthPops1000, monthPops4000;
    private double[] cellsInfectedTally500, cellsInfectedTally1000, cellsInfectedTally4000;
    private double[] cellsInfectedHistogram500, cellsInfectedHistogram1000, cellsInfectedHistogram4000;
    private final double TRANSMISSION_PROBABILITY = 0.49;
    private ArrayList<Double> stdDensities500, stdDensities1000, stdDensities4000, densities1000, positiveDensities1000; // need to allow multiple identical entries
    private HashMap<Integer, StrainInfo> strainInfoHashMap;
    private double[] densityHistogram500, densityHistogram1000, densityHistogram4000; // Each entry = number of cases at the density provided
    private double[] densityHistogram1000Raw;
    private double[] distinctDogsHistogram;
    private double[] strainROHistogram;
    private static double MEAN_DENSITY = 22.4; // Mean landscape density across time
    private int[] monthlyMaxCases1000;
    private double[] monthlySumStdDensity1000, monthlyMeanStdDensity1000;
    private ArrayList<Double> maxDistances;
    private double[] distancesHistogram;


    public static String toStringHeader() {
        return "TdMean,ThShape,ThScale,ThMean,incursionRate,densityDependence,meanDD,sdDD,maxDD,numDetectableDogs,numNonContributors,numSuperspreaders,R0,tsMW,density1000MW,positiveDensity1000MW,meanStdDensity500,meanStdDensity1000,meanStdDensity4000,stoppedEarly,stoppingReason,Param";
    }

    public String toString() {
        return TdMean + "," + ThShape + "," + ThScale+ "," + ThMean + "," + incursionRate + "," + densityDependence+ "," + meanDD + "," + sdDD + "," + maxDD + "," + numDetectableDogs + "," + numNonContributors + "," + numSuperspreaders + "," + R0 + "," + tsMW + "," + density1000MW + "," + positiveDensity1000MW + "," +
                meanStdDensity500 + "," + meanStdDensity1000 + "," + meanStdDensity4000 + "," + stoppedEarly + "," + stoppingReason + ",";
    }

    public int getBinIndex (double[] intervalMaxValues, double value) {
        for (int i = 0; i < intervalMaxValues.length; i++) {
            if (value <= intervalMaxValues[i]) { // Because this is how R does "hists" (i.e. includes the break in the bin below)
                return(i);
            }
        }
        return(-1);
    }
    public double[] getDistinctDogsHistogram() {
        return distinctDogsHistogram;
    }

    /**
     * @param monthBreaks double array of upper limits of number of days in each month (e.g. 31, 28, ...)
     * @param caseCountBreaks double array of upper limits of histogram bins of number of cases
     */
    public ABCOutcomes(double[] monthBreaks, double[] caseCountBreaks, double[] densityBreaks,
                       double[] distinctDogBreaks,
                       double[] densityRawBreaks, double[] strainR0Breaks, double[] distancesBreaks,
                       String fCaseList, double propExogenousNonBitersToInclude, double detectionProbability,
                       Boolean reallocateOutwithMP,
                       Metapopulation mp1000, String fProperties, EpidemicOutcomes ceo, double endInDays, Boolean hasOutcomes, String fDensitiesData, RandomEngine random) {

        System.out.println("In ABCOutcomes > Constructor and hasOutcomes = " + hasOutcomes);

        this.monthBreaks = monthBreaks;
        this.caseCountBreaks = caseCountBreaks;
        this.densityBreaks = densityBreaks;

        monthlyTally = new double[monthBreaks.length];
        monthlyHistogram = new double[caseCountBreaks.length];
        monthPops1000 = new MonthPops[monthBreaks.length];
        stdDensities1000 = new ArrayList<Double>();
        densities1000 = new ArrayList<Double>();
        positiveDensities1000 = new ArrayList<Double>(); // all densities > 0
        densityHistogram1000 = new double[densityBreaks.length];
        distinctDogsHistogram = new double[distinctDogBreaks.length];
        densityHistogram1000Raw = new double[densityRawBreaks.length];
        strainROHistogram = new double[strainR0Breaks.length];
        strainInfoHashMap = new HashMap<Integer, StrainInfo>();
        monthlyMaxCases1000 = new int[monthBreaks.length];
        monthlySumStdDensity1000 = new double[monthBreaks.length];
        monthlyMeanStdDensity1000 = new double[monthBreaks.length];
        maxDistances = new ArrayList<Double>(); // summary stat (e.g. max distance) of distances for each case (max distance to offspring cases), only including those with offspring cases
        distancesHistogram = new double[distancesBreaks.length];

        int nDD = 0;
        int monthIndex, caseCountIndex, popID1000;

        ep = new EpidemicProperties(fProperties);
        // If data then read in without case outcomes; otherwise, read in simulator output with case outcomes
        if (hasOutcomes) {
            caseList = CaseListReader.readCSVFileWithOutcomes("", fCaseList);
        } else {
            caseList = CaseListReader.readCSVFile(fCaseList);
        }
        System.out.println("In ABCOutcomes and have read in data from " + fCaseList + " and have " + caseList.size() + " entries.");
        double[] nDDvector = new double[caseList.size()];

        // Code to write out the densities at case locations from the data (and also standardised log density, etc.)
        CSVWriter DataDensitiesCSV = null;
        if(!hasOutcomes) {
            DataDensitiesCSV = new CSVWriter(fDensitiesData);
            DataDensitiesCSV.appendLine("CaseID, ParentID, StrainID, dogDensityK, Density1000, sld1000, x, y, positionAdjusted");
        }

        int numPositionAdjusted = 0;

        // Loop over cases in caseList and compile information on cases
        for (Case c : caseList) {

            // Compile information on all cases that were endogenously generated or made at least one bite
            if ((random.nextDouble() < detectionProbability) & ( c.getTypeOfCase() == Case.caseType.ENDOGENOUS || c.getOutcomes().getnDistinctDogs()>0 | random.nextDouble() < propExogenousNonBitersToInclude)) { // Incursion cases that didn't bite less likely to be recorded in the data

                // Compute distances to daughter cases
                ArrayList<Double> dists = c.computeDistancesToSecondary(caseList); // just for this case
                if (dists.size() > 0) {
                    maxDistances.add(new Statistics(dists).getMax());
                }

                // Get month for this case
                monthIndex = getBinIndex(monthBreaks, c.getDayInfectious());
                if (monthIndex >= 0) { // Ignore cases after endInDays which return -1 for monthIndex above
                    // Find population
                    if (reallocateOutwithMP) { // Reallocating if outwith grid according to arguments
                        c = mp1000.updateCaseClosestPopulationAll(c); // Reallocates spatially to nearest grid cell and provides new coordinates if outwith mp
                        popID1000 = c.getPopID();
                        if(c.getPositionAjusted()) { // tally up how many cases had their position adjusted
                            numPositionAdjusted ++;
                        }
                    } else { // Version where we don't provide a new location for this case
                        // System.out.println("Case ID: " + c.getId() + " " + c.getTypeOfCase() + " " + c.getPosition().toString());
                        popID1000 = mp1000.findPopulation(c.getPosition()).getPopID();
                    }

                    // Update monthlyTally, and PopMonth treeset
                    monthlyTally[monthIndex]++;
                    if (monthPops1000[monthIndex] == null) {
                        monthPops1000[monthIndex] = new MonthPops(popID1000, monthIndex);
                    } else {
                        monthPops1000[monthIndex].popIDTreeSet.add(popID1000);
                        if (monthPops1000[monthIndex].popCasesHashMap.containsKey(popID1000)) {
                            monthPops1000[monthIndex].popCasesHashMap.put(popID1000, monthPops1000[monthIndex].popCasesHashMap.get(popID1000)+1); // add one case in this population
                        } else {
                            monthPops1000[monthIndex].popCasesHashMap.put(popID1000, 1); // New hashmap (population) entry [used to tally up cases in this pop]
                        }
                    }

                    // Compute total dogs bitten (for R0 calculation)
                    nDD = nDD + c.getOutcomes().getnDistinctDogs();
                    if (c.getOutcomes().getnDistinctDogs() > maxDD) {
                        maxDD = c.getOutcomes().getnDistinctDogs();
                    }
                    if (c.getOutcomes().getnDistinctDogs() == 0) {
                        numNonContributors = numNonContributors+1;
                    }
                    nDDvector[numDetectableDogs] = (double)c.getOutcomes().getnDistinctDogs();

                    // Add standardised density for this case (builds up a list of standardised log densities of cases for this ABC run)
                    double daysUpdateK = 0;
                    if (monthIndex > 0) daysUpdateK = monthBreaks[monthIndex - 1]; // Correction for indexing not being the same in daysUpdateK and monthBreaks
                    double density1000 = mp1000.computeTimeConsistentDensity(popID1000, daysUpdateK);
                    double sld1000 = mp1000.computeStandardisedLogDensity(popID1000, daysUpdateK);
                    if (DataDensitiesCSV!=null) DataDensitiesCSV.appendLine(c.getId() + ", " + c.getParentID() + ", " + c.getStrainID() + ", " + c.getDogDensityK() + ", " + density1000 + ", " + sld1000 + ", " + c.getPosition().getX() + ", " + c.getPosition().getY() + ", " + c.getPositionAjusted().toString());
                    //System.out.println(c.getId() + ", " + c.getStrainID() + ", " + c.getDogDensityK() + ", " + density1000 + ", " + sld1000);

                    stdDensities1000.add(sld1000);
                    densities1000.add(density1000);

                    densityHistogram1000[getBinIndex(densityBreaks, sld1000)]++;
                    int nDDIndex = getBinIndex(distinctDogBreaks, c.getOutcomes().getnDistinctDogs());
                    if (nDDIndex != -1) { // Fix in case there are more distinct dogs bitten than in the histogram
                        distinctDogsHistogram[getBinIndex(distinctDogBreaks, c.getOutcomes().getnDistinctDogs())] ++;
                    } else {
                        distinctDogsHistogram[distinctDogBreaks.length-1] ++;
                    }
                    if (strainInfoHashMap.containsKey(c.getStrainID())) {
                        strainInfoHashMap.get(c.getStrainID()).addOneDogRecord(c.getOutcomes().getnDistinctDogs());
                    } else {
                        strainInfoHashMap.put(c.getStrainID(), new StrainInfo(c.getOutcomes().getnDistinctDogs(), 1));
                    }

                    densityHistogram1000Raw[getBinIndex(densityRawBreaks, density1000)] ++;
                    monthlySumStdDensity1000[monthIndex] += sld1000;

                } else {
                    System.out.println("Got a case after EndInDays " + c.getId());
                }
                numDetectableDogs ++;
            }

        }

        System.out.println("In ABCOutcomes > Constructor: Num positions adjusted: " + numPositionAdjusted);

        // Now compute the corresponding histograms, aggregating by month
        for (int mth = 0; mth < monthBreaks.length; mth ++) {
            // Get the number of cases in that month and update the histogram for that number of cases
            caseCountIndex = getBinIndex(caseCountBreaks, monthlyTally[mth]);
            monthlyHistogram[caseCountIndex] ++;

            // Cells infected tally by month
             if (monthPops1000[mth]==null) {
                monthlyMaxCases1000[mth] = 0;
            } else {
                monthlyMaxCases1000[mth] = monthPops1000[mth].getMax();
            }
            // Compute mean dog density for cases during the given month
            monthlyMeanStdDensity1000[mth] = monthlySumStdDensity1000[mth] / monthlyTally[mth];
        }

        // Make histogram of strain R0 values
        for (StrainInfo value : strainInfoHashMap.values()) {
           //System.out.println("In ABCOutcomes. value.getR0 = " + value.getR0());
           strainROHistogram[getBinIndex(strainR0Breaks, value.getR0())] ++;
        }

        // Make histogram of distances
        for (Double md: maxDistances) {
            distancesHistogram[getBinIndex(distancesBreaks, md)] ++;
        }

        // General information that can be added directly from EpidemicProperties
        if (ep != null) {
            ThShape = ep.getThShape();
            ThScale = ep.getThScale();
            ThMean = ThShape * ThScale;
            TdMean = ep.getTdMean();
            densityDependence = computeDD(MEAN_DENSITY, ThMean, TdMean);
        }
        R0 = ((double)nDD*TRANSMISSION_PROBABILITY) / numDetectableDogs; // Mean distinct dogs bitten * transmission prob ~0.49
        double[] nDDvectorDectectable = Arrays.copyOfRange(nDDvector, 0, numDetectableDogs);  // subset as there will be lots of zeros for non-detectable dogs
        Statistics numBites = new Statistics(nDDvectorDectectable);
        meanDD = numBites.getMean();
        sdDD = numBites.getStdDev();

        //Usage: poisson = new Poisson(meanDD); //qpois(p = 0.99, lambda = mu.hat, lower.tail = T, log.p = F)
        //double threshold = poisson.quantile(0.99, Boolean.TRUE, Boolean.FALSE);
        double threshold = Poisson.quantile (0.99, meanDD, Boolean.TRUE, Boolean.FALSE);
        for (Case c : caseList) {
            // Compile information on all cases that were endogenously generated or made at least one bite
            if (c.getOutcomes().getnDistinctDogs()>=threshold) { // Incursion cases that didn't bite won't meet threshold, so no need for separate test here unlikely
                numSuperspreaders += 1;
            }
        }

        meanStdDensity1000 = new Statistics(stdDensities1000).getMean();

        // Add positive densities only to the positive densities distribution
        for (Double d : densities1000) {
            if (d > 0) positiveDensities1000.add(d);
        }

        stoppedEarly = (ceo.getEndInDays() < endInDays);
        this.stoppingReason = ceo.getStoppingReason();

        // Close file to which we are writing out densities of cases in the data
        if (DataDensitiesCSV!=null) DataDensitiesCSV.close();
    }

    /**
     * # Computes the density dependence of the Holling Type II functional response at the density given
     * @return
     */
    private double computeDD(double dens, double Th, double Td) {
        double asymptote = 1/Th;
        // Proportion of asymptote achieved at tested value, from starting value
        double dd = 1 - ((MovementModel.computeExcursionRate(dens, Th, Td) - MovementModel.computeExcursionRate(0, Th, Td)) / (asymptote - MovementModel.computeExcursionRate(0, Th, Td)));
        return (dd);
    }

    public double[] getMonthlyTally() {
        return monthlyTally;
    }

    public double[] getCellsInfectedTally500() {
        return cellsInfectedTally500;
    }
    public double[] getCellsInfectedTally1000() {
        return cellsInfectedTally1000;
    }
    public double[] getCellsInfectedTally4000() {
        return cellsInfectedTally4000;
    }

    public double[] getDensityHistogram500() {
        return densityHistogram500;
    }
    public double[] getDensityHistogram1000() {
        return densityHistogram1000;
    }
    public double[] getDensityHistogram4000() {
        return densityHistogram4000;
    }

    public double[] getDensityHistogram1000Raw() {
        return densityHistogram1000Raw;
    }
    public double[] getStrainR0Histogram() {
        return strainROHistogram;
    }

    public double[] getMonthlyHistogram() {
        return monthlyHistogram;
    }
    public double[] getCellsInfectedHistogram500() {
        return cellsInfectedHistogram500;
    }
    public double[] getCellsInfectedHistogram1000() {
        return cellsInfectedHistogram1000;
    }
    public double[] getCellsInfectedHistogram4000() {
        return cellsInfectedHistogram4000;
    }
    public double[] getMonthlyMeanStdDensity1000() { return monthlyMeanStdDensity1000; }
    public ArrayList<Double> getPositiveDensities1000() { return positiveDensities1000; }
    public ArrayList<Double> getDensities1000() { return densities1000;}
    public int[] getMonthlyMaxCases1000() { return monthlyMaxCases1000; }
    public double[] getDistancesHistogram() { return distancesHistogram; }

    public void computeStatistics( ABCOutcomes abcReal ) {
        // Computations for statistical tests for ABC step
        MannWhitneyTest mwtMonthlyTally = new MannWhitneyTest(monthlyTally, abcReal.getMonthlyTally(), NOT_EQUAL);
        tsMW = mwtMonthlyTally.approxSP(); // timeseries Mann-Whitney test

        MannWhitneyTest mwtDensity1000Raw = new MannWhitneyTest(Util.ArrayListDouble2doubleArr(densities1000), Util.ArrayListDouble2doubleArr(abcReal.getDensities1000()), NOT_EQUAL);
        density1000MW = mwtDensity1000Raw.approxSP(); // density at case locations Mann-Whitney
        MannWhitneyTest mwtPositiveDensity1000 = new MannWhitneyTest(Util.ArrayListDouble2doubleArr(positiveDensities1000), Util.ArrayListDouble2doubleArr(abcReal.getPositiveDensities1000()), NOT_EQUAL);
        positiveDensity1000MW = mwtPositiveDensity1000.approxSP();
    }


    private class MonthPops implements Comparable<MonthPops> {
        int monthIndex;
        TreeSet<Integer> popIDTreeSet;
        HashMap<Integer, Integer> popCasesHashMap;

        private MonthPops (int popID, int monthIndex) {
            this.monthIndex = monthIndex;
            popIDTreeSet = new TreeSet<Integer>();
            popIDTreeSet.add(popID);
            popCasesHashMap = new HashMap<Integer, Integer>();
            popCasesHashMap.put(popID, 1); // Keeps track of the count of cases in this population in this month
        }

        private int getMax() {
            int currMax = 0;
            for (Integer i : popCasesHashMap.values()) {
                if (i > currMax) currMax = i;
            }
            return currMax;
        }

        /**
         * Orders PopMonth first by month, then by population
         * @param other The PopMonth to compare with
         * @return comp comparison
         */
        public int compareTo(final MonthPops other) {
            if (other == null) {
                return 1;
            }
            final int comp = this.monthIndex - other.monthIndex;
            return comp;
        }
    }


    private class StrainInfo {
        int nDistinctDogsBitten;
        int nCases;

        private StrainInfo(int nDistinctDogsBitten, int nCases) {
            this.nDistinctDogsBitten = nDistinctDogsBitten;
            this.nCases = nCases;
        }

        private void addDogsBitten (int n) {
            this.nDistinctDogsBitten = this.nDistinctDogsBitten + n;
        }
        private void addCases (int n) {
            this.nCases = this.nCases + n;
        }
        private void addOneDogRecord (int bitesMade) {
            this.nCases ++;
            this.nDistinctDogsBitten = this.nDistinctDogsBitten + bitesMade;
        }
        private int getnDistinctDogsBitten () {
            return nDistinctDogsBitten;
        }
        private double getR0() {
            return TRANSMISSION_PROBABILITY * ((double) nDistinctDogsBitten / (double) nCases);
        }
    }

}
