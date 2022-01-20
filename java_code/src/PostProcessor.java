/**
 * PostProcessor combines outcomes from multiple runs, and compiles these into the format required for the ABC steps
 **/

import jdistlib.rng.MersenneTwister;
import jdistlib.rng.RandomEngine;
import org.nocrala.tools.gis.data.esri.shapefile.exception.InvalidShapeFileException;
import java.io.IOException;
import java.util.ArrayList;

public class PostProcessor {
    static RandomEngine random = new MersenneTwister(123L);
    private final static String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces

    public static void main(String[] args) throws InvalidShapeFileException, com.vividsolutions.jts.io.ParseException, IOException {

        // ----------------------------- Main editable section -----------------------------
        // Folder baseline information
        String home = System.getProperty("user.home") + "/Developer/Rabies_Sim_v02/Rabies_Sim_Output_Test/";
        String resolution = "1000";
        String simStructure = ""; // Any subfolder structure to use
        String experimentalSetup = "Round0";
        int nParamSets = 1;
        Boolean includeTicToc = Boolean.FALSE; // whether to include run-time information in overall epidemic outcomes file
        double propExogenousNonBitersToInclude = 1.0; // Superseded (now always set to 1.0)
        double detectionProbability = 1.0; // overall detection probability - Superseded (now always set to 1.0 because detection rates shown to be high)
        // --------------------------- End of editable section ---------------------------

        int minParamID = 1, maxParamID = (minParamID-1)+nParamSets;
        // Computed folders
        String experimentID = experimentalSetup + "_" + resolution;
        String simFolder = home + simStructure + experimentID + "/"; // Baseline folder for simulation output, parameter files, etc.
        System.out.println(simFolder);
        String postprocessingFolder = simFolder + "postprocessing/";
        String fEpidemicOutcomes = simFolder + "output_files/epi_outcomes_" +  String.format("%03d", minParamID) + "_" +  String.format("%03d", maxParamID) + ".csv";

        EpidemicOutcomes cEO = new EpidemicOutcomes(); // current EpidemicOutcomes
        String fDataCaseList = home + "matching_data/d_cases_java.csv";

        // Set up storage for aggregated data (e.g. as binned counts/ histograms), by setting breaks (bin edges) and CSVWriter objects
        double endInDays = 5112; // 6939 days until end 2020, 5112 is the normal run
        double[] monthBreaks = {31,59,90,120,151,181,212,243,273,304,334,365,396,424,455,485,516,546,577,608,638,669,699,730,761,790,821,851,882,912,943,974,1004,1035,1065,1096,1127,1155,1186,1216,1247,1277,1308,1339,1369,1400,1430,1461,1492,1520,1551,1581,1612,1642,1673,1704,1734,1765,1795,1826,1857,1885,1916,1946,1977,2007,2038,2069,2099,2130,2160,2191,2222,2251,2282,2312,2343,2373,2404,2435,2465,2496,2526,2557,2588,2616,2647,2677,2708,2738,2769,2800,2830,2861,2891,2922,2953,2981,3012,3042,3073,3103,3134,3165,3195,3226,3256,3287,3318,3346,3377,3407,3438,3468,3499,3530,3560,3591,3621,3652,3683,3712,3743,3773,3804,3834,3865,3896,3926,3957,3987,4018,4049,4077,4108,4138,4169,4199,4230,4261,4291,4322,4352,4383,4414,4442,4473,4503,4534,4564,4595,4626,4656,4687,4717,4748,4779,4807,4838,4868,4899,4929,4960,4991,5021,5052,5082,5112};
        double[] caseCountBreaks = {0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95,100,105,110,115,120,125,130,135,140,145,150,155,160,165,170,175,180,185,190,195,200,205,210,215,220,225,230,235,240,245,250,100000};
        double[] densityBreaks = {-2.00,-1.75,-1.50,-1.25,-1.00,-0.75,-0.50,-0.25,0.00,0.25,0.50,0.75,1.00,1.25,1.50,1.75,2.00,2.25,2.50,2.75,3.00,3.25,3.50,3.75,4.00};
        double[] distinctDogBreaks = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,10000};
        double[] densityRawBreaks = {0.0001,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300,310,320,330,340,350,360,370,380,390,400,410,420,430,440,450,460,470,480,490,500,550,600,650,700,750,800,850,900,950,1000,1050,1100,1150,1200,1250,1300,1350,1400,1450,1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050,2100,2150,2200,2250,2300,2350,2400,2450,2500};
        double[] strainR0Breaks = {0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10000};
        double[] distancesBreaks = {100,200,300,400,500,600,1000,4000,10000,100000};

        String paramRangeString = String.format("%03d", minParamID) + "_" +  String.format("%03d", maxParamID);
        String fABC = postprocessingFolder + "abc_" + paramRangeString + ".csv";
        String fMonthlyTally = postprocessingFolder + "monthlyTally_" + paramRangeString + ".csv";
        String fMonthlyHistogram = postprocessingFolder + "monthlyHistogram_" + paramRangeString + ".csv";
        String fDensityHistogram1000 = postprocessingFolder + "densityHistogram1000_" + paramRangeString + ".csv";
        String fDensityHistogram1000Raw = postprocessingFolder + "densityHistogram1000Raw" + paramRangeString + ".csv";
        String fDistinctDogHistogram = postprocessingFolder + "distinctDogs_" + paramRangeString + ".csv";
        String fStrainR0Histogram = postprocessingFolder + "strainR0_" + paramRangeString + ".csv";
        String fMonthlyMaxCases1000 = postprocessingFolder + "monthlyMaxCases1000_" + paramRangeString + ".csv";
        String fMonthlyMeanStdDensity1000 = postprocessingFolder + "monthlyMeanStdDensity1000_" + paramRangeString + ".csv";
        String fDistancesHistogram = postprocessingFolder + "distancesHistogram_" + paramRangeString + ".csv";
        String fDensitiesData = postprocessingFolder + "densitiesData.csv"; // Output densities to this file

        ArrayList<EpidemicOutcomes> eo;
        System.out.println("Epidemic outcomes file: " + fEpidemicOutcomes);
        eo = EpidemicOutcomesReader.readEpidemicOutcomes(fEpidemicOutcomes, includeTicToc);
        System.out.println("Number of eo read in: " + eo.size());

        // Set up writers for writing output
        CSVWriter abcWriter = new CSVWriter(fABC); abcWriter.appendLine(ABCOutcomes.toStringHeader());
        CSVWriter monthlyTallyWriter = new CSVWriter(fMonthlyTally); monthlyTallyWriter.appendLine(Util.dblArr2csv(monthBreaks) + "Param");
        CSVWriter monthlyHistogramWriter = new CSVWriter(fMonthlyHistogram); monthlyHistogramWriter.appendLine(Util.dblArr2csv(caseCountBreaks) + "Param");
        CSVWriter densityHistogramWriter1000 = new CSVWriter(fDensityHistogram1000); densityHistogramWriter1000.appendLine(Util.dblArr2csv(densityBreaks) + "Param");
        CSVWriter distinctDogHistogramWriter = new CSVWriter(fDistinctDogHistogram); distinctDogHistogramWriter.appendLine(Util.dblArr2csv(distinctDogBreaks) + "Param");
        CSVWriter densityHistogramWriter1000Raw = new CSVWriter(fDensityHistogram1000Raw); densityHistogramWriter1000Raw.appendLine(Util.dblArr2csv(densityRawBreaks) + "Param");
        CSVWriter strainR0HistogramWriter = new CSVWriter(fStrainR0Histogram); strainR0HistogramWriter.appendLine(Util.dblArr2csv(strainR0Breaks) + "Param");
        CSVWriter monthlyMaxCases1000Writer = new CSVWriter(fMonthlyMaxCases1000); monthlyMaxCases1000Writer.appendLine(Util.dblArr2csv(monthBreaks));
        CSVWriter monthlyMeanStdDensity1000Writer = new CSVWriter(fMonthlyMeanStdDensity1000); monthlyMeanStdDensity1000Writer.appendLine(Util.dblArr2csv(monthBreaks));
        CSVWriter distancesHistogramWriter = new CSVWriter(fDistancesHistogram); distancesHistogramWriter.appendLine(Util.dblArr2csv(distancesBreaks) + "Param");

        // Set up the Metapopulation object
        String fPops1000 = home + "static_files/pops_1000.csv"; String fWKT1000 = home + "static_files/grd_1000.txt";
        Metapopulation mp1000 = new Metapopulation(fPops1000, fWKT1000, "1000", 1000*1000, random, Main.rgen); mp1000.setStdDensity();

        // ----- Compute ABCOutcomes for real data -----
        System.out.println("Computing ABC outcomes for real data");
        String fProperties = home + "matching_data/params_data.properties"; // Placeholder file to make it function
        ABCOutcomes abcReal = new ABCOutcomes(monthBreaks, caseCountBreaks, densityBreaks, distinctDogBreaks, densityRawBreaks,
                strainR0Breaks, distancesBreaks, fDataCaseList, 1.0, 1.0, Boolean.FALSE,
                mp1000, fProperties, cEO, endInDays, Boolean.TRUE, fDensitiesData, random); // cEO is empty at this stage
        abcWriter.appendLine(abcReal.toString() + "data");
        monthlyTallyWriter.appendLine(Util.dblArr2csv(abcReal.getMonthlyTally()) + "data");
        monthlyHistogramWriter.appendLine(Util.dblArr2csv(abcReal.getMonthlyHistogram()) + "data");
        densityHistogramWriter1000.appendLine(Util.dblArr2csv(abcReal.getDensityHistogram1000()) + "data");
        distinctDogHistogramWriter.appendLine(Util.dblArr2csv(abcReal.getDistinctDogsHistogram()) + "data");
        densityHistogramWriter1000Raw.appendLine(Util.dblArr2csv(abcReal.getDensityHistogram1000Raw()) + "data");
        strainR0HistogramWriter.appendLine(Util.dblArr2csv(abcReal.getStrainR0Histogram()) + "data");
        monthlyMaxCases1000Writer.appendLine(Util.intArr2csv(abcReal.getMonthlyMaxCases1000()) + "data");
        monthlyMeanStdDensity1000Writer.appendLine(Util.dblArr2csv(abcReal.getMonthlyMeanStdDensity1000()) + "data");
        distancesHistogramWriter.appendLine(Util.dblArr2csv(abcReal.getDistancesHistogram()) + "data");

        ArrayList<Case> caseList = CaseListReader.readCSVFile(fDataCaseList);
        int monthIndex, popID1000; double daysUpdateK = 0;
        for (Case c : caseList) {
            // Get month for this case
            monthIndex = abcReal.getBinIndex(monthBreaks, c.getDayInfectious());
            popID1000 = mp1000.findPopulationAll(c.getPosition()).getPopID();
            if (monthIndex > 0) daysUpdateK = monthBreaks[monthIndex - 1]; // Correction for indexing not being the same in daysUpdateK and monthBreaks
            c.setDogDensityK(mp1000.computeTimeConsistentDensity(popID1000, daysUpdateK));
            // Output caselist with standardised log densities for use in ABC step
            c.setDogDensityK(mp1000.computeStandardisedLogDensity(popID1000,daysUpdateK));
        }

        // Get simulated data
        for (int paramID = minParamID; paramID <= maxParamID; paramID ++) {
            // Set up the current parameter set information
            String formattedParamID = String.format("%03d", paramID);
            String formattedRunID = String.format("%03d", 1);
            cEO = eo.get(paramID-minParamID);
            System.out.println("Computing ABC outcomes for EpidemicOutcomes: " + paramID + "=" + cEO.getParamID());

            // Set filepaths for properties and caselist
            fProperties = simFolder + "input_files/" + "params_" + formattedParamID + ".properties";
            String fCaseList = simFolder + "output_files/" + experimentID + "_sim_cases_" + formattedParamID + ".csv";

            // ----- Compute abc outcomes for this run -----
            ABCOutcomes abc = new ABCOutcomes(monthBreaks, caseCountBreaks, densityBreaks, distinctDogBreaks,densityRawBreaks, strainR0Breaks, distancesBreaks,
                    fCaseList, propExogenousNonBitersToInclude, detectionProbability, Boolean.TRUE, mp1000, fProperties, cEO, endInDays, Boolean.TRUE, "", random);
            abc.computeStatistics(abcReal);

            // Write outcomes to file
            abcWriter.appendLine(abc.toString() + formattedParamID);
            monthlyTallyWriter.appendLine(Util.dblArr2csv(abc.getMonthlyTally()) + formattedParamID);
            monthlyHistogramWriter.appendLine(Util.dblArr2csv(abc.getMonthlyHistogram()) + formattedParamID);
            densityHistogramWriter1000.appendLine(Util.dblArr2csv(abc.getDensityHistogram1000()) + formattedParamID);
            distinctDogHistogramWriter.appendLine(Util.dblArr2csv(abc.getDistinctDogsHistogram()) + formattedParamID);
            densityHistogramWriter1000Raw.appendLine(Util.dblArr2csv(abc.getDensityHistogram1000Raw()) + formattedParamID);
            strainR0HistogramWriter.appendLine(Util.dblArr2csv(abc.getStrainR0Histogram()) + formattedParamID);
            monthlyMaxCases1000Writer.appendLine(Util.intArr2csv(abc.getMonthlyMaxCases1000()) + formattedParamID);
            monthlyMeanStdDensity1000Writer.appendLine(Util.dblArr2csv(abc.getMonthlyMeanStdDensity1000()) + formattedParamID);
            distancesHistogramWriter.appendLine(Util.dblArr2csv(abc.getDistancesHistogram()) + formattedParamID);
        }
        // Close writers
        abcWriter.close(); monthlyTallyWriter.close(); monthlyHistogramWriter.close();
        densityHistogramWriter1000.close();
        distinctDogHistogramWriter.close(); densityHistogramWriter1000Raw.close(); strainR0HistogramWriter.close(); monthlyMaxCases1000Writer.close();
        monthlyMeanStdDensity1000Writer.close(); distancesHistogramWriter.close();

    }

}
