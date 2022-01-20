/**
 * Generate properties files to initialise a set of runs
 */

import jdistlib.rng.MersenneTwister;
import jdistlib.rng.RandomEngine;
import org.nocrala.tools.gis.data.esri.shapefile.exception.InvalidShapeFileException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

public class Initialise {

    private final static String CSV_DELIMITER = ", *|\\n|\\n\\r";       // delimiters in csv file are new lines and commas + arbitrary num of spaces
    static RandomEngine random = new MersenneTwister(123L);
    static Random rgen = new Random(123L);
    private static final double M2_PER_KM2 = 1000*1000;
    private enum RunType {NORMAL, LOWER_VAX, ZERO_VAX, R0_CALC, R0_MULTI, R0_MULTI_LOCATION, NAIVE_POP, WO_DOG_VARIATION, STOP_INCURSIONS}

    public static void main(String[] args) throws InvalidShapeFileException, com.vividsolutions.jts.io.ParseException, IOException {

        // ----------------------------- Editable section -----------------------------
        // Folder and baseline information
        String home = System.getProperty("user.home") + "/Developer/Rabies_Sim_v02/Rabies_Sim_Output_Test/";
        String experimentalSetup = "Round0";
        String model_scale = "1000";
        RunType runType = RunType.NORMAL;
        String simStructure = "";
        int nParamSets = 1;
        // --------------------------- End of editable section ---------------------------

        String resolution;
        if(model_scale.equals("Data")) {
            resolution = "1000";
        } else {
            resolution = model_scale;
        }

        // Set parameters according to runType held in "additionalString". Note that some of these are redundant
        // but are included for back-compatibility and/or clarity
        String fCampaigns, additionalString = "";
        Boolean simulatedInitialCases = Boolean.FALSE, includeIncursions = Boolean.FALSE,
                overwriteIncursions = Boolean.FALSE, allowEndogenousToTransmit = Boolean.FALSE;
        int maxCases = 10000; // approx 3281 observed cases x 3
        switch (runType) {
            case LOWER_VAX:
                fCampaigns = "vc_third_" + resolution + ".csv";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.TRUE;
                additionalString = RunType.LOWER_VAX.name();
                maxCases = 50000;
                break;
            case ZERO_VAX:
                fCampaigns = "";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.TRUE;
                additionalString = RunType.ZERO_VAX.name();
                maxCases = 100000;
                break;
            case R0_CALC:
                fCampaigns = "";
                simulatedInitialCases = Boolean.TRUE;
                includeIncursions = Boolean.FALSE;
                allowEndogenousToTransmit = Boolean.FALSE;
                additionalString = RunType.R0_CALC.name();
                break;
            case NAIVE_POP:
                fCampaigns = "";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.FALSE;
                additionalString = RunType.NAIVE_POP.name();;
                break;
            case R0_MULTI:
                fCampaigns = "";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.FALSE;
                additionalString = RunType.R0_MULTI.name();
                maxCases = 100000;
                break;
            case R0_MULTI_LOCATION:
                fCampaigns = "";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.FALSE;
                additionalString = RunType.R0_MULTI_LOCATION.name();
                maxCases = 100000;
                break;
            case STOP_INCURSIONS:
                fCampaigns = "vc_" + resolution + ".csv";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.TRUE;
                additionalString = RunType.STOP_INCURSIONS.name();
                break;
            case WO_DOG_VARIATION:
                fCampaigns = "vc_" + resolution + ".csv";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.TRUE;
                additionalString = RunType.WO_DOG_VARIATION.name();
                maxCases = 50000;
                break;
            default:  // NORMAL
                fCampaigns = "vc_" + resolution + ".csv";
                simulatedInitialCases = Boolean.FALSE;
                includeIncursions = Boolean.TRUE;
                allowEndogenousToTransmit = Boolean.TRUE;
                additionalString = RunType.NORMAL.name();
                break;
        }
        overwriteIncursions = Boolean.FALSE;
        // Print out type of run for checking
        System.out.println(additionalString);

        // Computed folders
        String fInitialCases = home + "static_files/" + "d_initial_cases_java.csv"; // Default value; overwritten if simulatedInitialCases is TRUE

        String experimentID = experimentalSetup + "_" + model_scale;
        if (runType != RunType.NORMAL) {
            experimentID = experimentalSetup + "_" + additionalString + "_" + model_scale;
        }

        String simFolder = home + simStructure + experimentID + "/"; // Baseline folder for simulation output, parameter files, etc.
        String fPriors = simFolder + experimentalSetup + "_priors_" + model_scale + ".csv";
        if (runType != RunType.NORMAL) {
            fPriors = simFolder + experimentalSetup + "_" + additionalString + "_priors_" + model_scale + ".csv";
        }
        Boolean samplePriors = Boolean.TRUE;
        String folderIncursionCases = simFolder + "incursions/";
        String staticFilesFolder = home + "static_files/";
        String folderEpidemicOutcomes = simFolder + "output_files/";
        String propertiesFolder = simFolder + "input_files/";
        // Fixed parameters (for back-compatibility)
        String movementModelName = "fixedDensityDependence";
        String epidemiologicalModelName = "sampledIncubationInfectiousPeriods", epidemiologicalParameterValues = "d_inc_inf_with_zeros.csv";

        System.out.println(simFolder);

        // Compute additional parameters using the above
        double totalAreaSqKm = 2474.25;
        double cellAreaSqM;
        if (resolution.equals("all")) {
            cellAreaSqM = totalAreaSqKm * M2_PER_KM2;
        } else if (resolution.equals("Data")) {
            cellAreaSqM = 1.0 * M2_PER_KM2; // resolution 1000, thus cell area 1km2, or 1,000,000 m2
        } else {
            cellAreaSqM = Integer.parseInt(resolution) * Integer.parseInt(resolution);
        }
        System.out.println("In Main_Initial and cellAreaSqM =" + cellAreaSqM);

        // Compute max parameter set ID
        int minParamID = 1, maxParamID = (minParamID-1)+nParamSets;

        // Read in priors
        ParamSampler ps = new ParamSampler("", fPriors, samplePriors);
        System.out.println(experimentID);
        ArrayList<ParamSet> sampledParams = ps.sampleParams(nParamSets, random, experimentID);
        System.out.println("Number of sampled parameters: " + sampledParams.size() + " NB: there may be more than this available in the file.");

        // Set up epidemic properties file/info for each run
        for (int paramID = minParamID; paramID <= maxParamID; paramID ++) {
            String formattedParamID = String.format("%03d", paramID);
            System.out.println("ParamID = " + formattedParamID);

            // Create and write out a new .properties file for each parameter set
            String fProperties = simFolder + "input_files/" + "params_" + formattedParamID + ".properties";
            System.out.println("fProperties:" + fProperties);

            // Set up EpidemicProperties object (NB: other attributes are added to this later)
            System.out.println("In Main_Initial > Main and resolution = " + resolution);
            EpidemicProperties ep = new EpidemicProperties(fProperties, experimentID, propertiesFolder, folderEpidemicOutcomes, folderIncursionCases, additionalString,
                    paramID, minParamID, epidemiologicalModelName, movementModelName, resolution, allowEndogenousToTransmit,
                    epidemiologicalParameterValues,
                    fCampaigns, sampledParams, home, totalAreaSqKm, maxCases, random);

            // Set up the Metapopulation object and EpidemiologicalModel
            Metapopulation mp = new Metapopulation(ep.getStaticFilesFolder() + ep.getfPops(), ep.getStaticFilesFolder() + ep.getfWKT(), ep.getAreaName(), cellAreaSqM, random, rgen);
            System.out.println("Number of villages: " + mp.getspatialUnitHashMap().size());
            EpidemiologicalModel epiModel;
            epiModel = new EpidemiologicalModel(ep.getStaticFilesFolder(), ep.getfEpiVariables(), ep.getepidemiologicalModelName(), ep.getTransGivenBiteProb(), random);

            // Set filenames for incursions and initial cases, according to runType
            if (runType.equals(runType.NAIVE_POP) || runType.equals(runType.R0_MULTI) || runType.equals(runType.R0_MULTI_LOCATION)) { // incursions different per run
                String fincursions = folderIncursionCases + "incursions_" + formattedParamID + ".csv";
                ep.setfIncursions(fincursions);
            } else if (runType.equals(runType.R0_CALC)) { // incursions not included, and initial cases simulated
                ep.setfIncursions(""); // Leave field blank if no incursions
                // Simulate initial cases so that R0 can be computed
                int nInitialCases = 1;
                double startInitialInterval = 0;
                double endInitialInterval = 5112;
                ArrayList<Case> initialCaseList;  // Storage for initial cases
                initialCaseList = makeInitialCases(mp, epiModel, startInitialInterval, endInitialInterval, nInitialCases, ep.getInitialLocationsType());
                // Write out the initialCaseList
                fInitialCases = ep.getInputFolder() + "initial_cases_" + formattedParamID + ".csv";
                System.out.println(fInitialCases);
                CaseListReader.writeCsvFile(fInitialCases, initialCaseList);
            } else { // incursions are the same for all runs in this folder
                String fincursions = folderIncursionCases + "incursions_java" + ".csv";
                ep.setfIncursions(fincursions);
            }
            ep.setfInitialCases(fInitialCases); // add file path to properties file

            // Write properties to disk
            ep.writeProperties(fProperties);
        }
    }

    /**
     * Makes initial case list (used in R0 calculation version of code).
     * @param mp Metapopulation
     * @param epiModel Epidemiological Model
     * @param startInitialInterval start of interval for simulating initial cases
     * @param endInitialInterval end of interval for simulating initial cases
     * @param nInitialCases number of cases to simulate
     * @param initialLocationsType How to spatially allocate initial cases to grid cells
     * @return
     */
    private static ArrayList<Case> makeInitialCases (Metapopulation mp, EpidemiologicalModel epiModel, double startInitialInterval, double endInitialInterval, int nInitialCases,
                                                     String initialLocationsType){
        int nCases = 0;
        Population currPop;
        Case currCase;
        double currDayGenerated;
        int strainID = 0;

        ArrayList<Case> initialCaseList = new ArrayList<Case>(); // Storage for initial cases

        while (nCases < nInitialCases) {

            currPop = null;
            if (initialLocationsType.equals("randUnif")) {
                currPop = mp.getRandPopAboveX(0); // unif random across cells with K>0
            } else if (initialLocationsType.equals("randPropK")) {
                currPop = mp.getRandPopProportionalK();
            } else if (initialLocationsType.equals("randPropLogK")) {
                currPop = mp.getRandPopProportionalLogK();
            } else if (initialLocationsType.equals("randUnifAboveMedian")) {
                currPop = mp.getRandPopAboveX(mp.getMedianSize());
            } else {
                System.err.println("From Main_Initial: initialLocationsType parameter = " + initialLocationsType + " not recognised.");
                System.exit(0);
            }

            if (currPop.getK() == 0) {
                System.err.println("Tried to create an initial case in a population wiht no dogs: " + currPop.toString());
            }
            if (currPop.getDogsS() > 0) {
                // Make one of the dogs there Exposed
                currPop.doInfection();
                currDayGenerated = Util.randInInterval(startInitialInterval, endInitialInterval, random);
                // Make a new case and add it to the initialCaseList, at location and in the selected population
                currCase = new Case(currDayGenerated, epiModel.randEpiVariableSet(), mp.generateRandomPoint(currPop), currPop, strainID, Case.caseType.INITIAL);
                initialCaseList.add(currCase);
                nCases ++;
                strainID++;
            }

        }
        return initialCaseList;
    }

}
