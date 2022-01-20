/**
 * Class Main contains the code to run at set of simulations, after these have been set up using Initialise
 */

import jdistlib.rng.MersenneTwister;
import jdistlib.rng.RandomEngine;
import org.nocrala.tools.gis.data.esri.shapefile.exception.InvalidShapeFileException;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import java.util.Random;

public class Main {

    static RandomEngine random = new MersenneTwister(123L);
    static Random rgen = new Random(123L);

    private final static double M2_PER_KM2 = 1000*1000;
    private final static String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces

    public static void main(String[] args) throws InvalidShapeFileException, com.vividsolutions.jts.io.ParseException, IOException {

        // ----------------------------- Main editable section -----------------------------
        // Folder baseline information
        String home = System.getProperty("user.home") + "/Developer/Rabies_Sim_v02/Rabies_Sim_Output_Test/";
        String experimentalSetup = "Round0";
        String model_scale = "1000";
        String simStructure = "";
        // Set start and end run ids (each with a set of parameters drawn with replacement from the priors/posteriors, etc.)
        int nParamSets = 1;
        int minParamID = 1, maxParamID = (minParamID-1)+nParamSets;
        ///int minParamID = 619, maxParamID = 623; // or use this line to (re-)run particular ones

        Boolean testRun = Boolean.FALSE, stopIfExtinct = Boolean.TRUE;

        // Note that fMixedPopCoverageOutput and fByPopCoverageOutput can be edited below to
        // include output on vaccination coverage. NB: If fByPopCoverageOutput is written out, this generates
        // very large files.
        // --------------------------- End of editable section ---------------------------

        // Compute additional parameters using the above
        String resolution;
        if(model_scale.equals("Data")) {
            resolution = "1000";
        } else {
            resolution = model_scale;
        }
        double totalAreaSqKm = 2474.25;
        double cellAreaSqM;
        if (resolution.equals("all")) {
            cellAreaSqM = totalAreaSqKm * M2_PER_KM2;
        } else if (resolution.equals("Data")) {
            cellAreaSqM = 1.0 * M2_PER_KM2; // resolution 1000, thus cell area 1km2, or 1,000,000 m2
        } else {
            cellAreaSqM = Integer.parseInt(resolution) * Integer.parseInt(resolution);
        }
        System.out.println("In Main and cellAreaSqM =" + cellAreaSqM);

        // Computed folders (Note that initial cases information comes from EpidemicProperties)
        String experimentID = experimentalSetup + "_" + model_scale; // e.g. "infData_FixedDD_1000"
        String simFolder = home + simStructure + experimentID + "/"; // Baseline folder for simulation output, parameter files, etc.
        String fEpidemicOutcomes = simFolder + "/output_files/epi_outcomes_" +  String.format("%03d", minParamID) + "_" +  String.format("%03d", maxParamID) + ".csv";
        String propertiesFolder = simFolder + "input_files/";
        String folderFinalPops = ""; ///home + experimentID + "/output_files/"; // Set to "" to avoid outputting this
        Boolean includeTicToc = Boolean.FALSE; // whether to include run-time information in overall epidemic outcomes file

        // Check file paths before we get any further
        System.out.println("home:" + home); System.out.println("experimentID:" + experimentID); System.out.println("fEpidemicOutcomes:" + fEpidemicOutcomes); System.out.println("parametersFolder:" + propertiesFolder); System.out.println("folderFinalPops:" + folderFinalPops);
        System.out.println(simFolder);
        ///System.exit(0);

        // Set things up and run simulator
        EpidemicProperties ep;
        EpidemicOutcomes eo = new EpidemicOutcomes();

        CSVWriter eoWriter = new CSVWriter(fEpidemicOutcomes); // Overall outcomes file - all runs combined
        eoWriter.appendLine(eo.getHeader(includeTicToc));

        // -------------------------- Main simulation loop --------------------------------
        // Loop over parameter ids (i.e. run numbers), and simulate epidemics
        for (int paramID = minParamID; paramID <= maxParamID; paramID ++) {
            System.out.println("---------------------------------New Simulation-------------------------------------");
            // Set up the current parameter set
            String formattedParamID = String.format("%03d", paramID);

            String fProperties = propertiesFolder + "params_" + formattedParamID + ".properties";
            ep = new EpidemicProperties(fProperties);

            // Check consistency of epidemic properties file (generated by Main_Initial) and cellAreaM (made from editable "resolution" above)
            if (ep.getCellAreaSqKm()*(1000*1000) != cellAreaSqM) {
                // Abort if non-match found
                System.out.println("Cell area not consistent in properties file and editable parameters in Main: ep.getCellAreaSqKm= " + ep.getCellAreaSqKm() + " and cellAreaM=" + cellAreaSqM);
                System.exit(0);
            }

            // Set up the DemographicModel, Metapopulation and EpidemiologicalModel objects
            DemographicModel dm = new DemographicModel(ep.getPerCapBirthRate(), ep.getPerCapDeathRate(), resolution, random);
            System.out.println("Reading in populations and geometries from: " + ep.getfPops() + " and " + ep.getfWKT());
            Metapopulation mp = new Metapopulation(ep.getStaticFilesFolder()+ep.getfPops(), ep.getStaticFilesFolder()+ep.getfWKT(), ep.getAreaName(), cellAreaSqM, random, rgen);
            dm.computeDemographicRates(mp);
            System.out.println("There are " + mp.getNumGeometries() + " geometries in " + mp.getMpName() + " and the total area is " + mp.getKM2() + " km^2");
            EpidemiologicalModel epiModel;
            epiModel = new EpidemiologicalModel(ep.getStaticFilesFolder(), ep.getfEpiVariables(), ep.getepidemiologicalModelName(), ep.getTransGivenBiteProb(), random);

            MovementModel mvModel = new MovementModel(mp, ep.getMovementModelName(), ep.getSkShape(), ep.getSkScale(), ep.getLongDistProb(),
                    ep.getTdMean(), ep.getThShape(), ep.getThScale(), ep.getAdditionalString(),
                    ep.getCellAreaSqKm(), ep.getLongDistanceLocationsType(), random);

            // Reset the Demographic Model (i.e. metapopulation dog status information and demographic rates), ready for a new epidemic
            mp.reset(dm);

            // Run-specific output information (fMixedPopCoverageOutput and fByPopCoverageOutput can be edited)
            String fMixedPopCoverageOutput = ""; // ep.getOutputFolder() + experimentID + "_mixedpop_coverage_" + formattedParamID + ".csv";
            String fByPopCoverageOutput = "";
            System.out.println("fMixedPopCoverageOutput = " + fMixedPopCoverageOutput);
            String fCaseList = ep.getOutputFolder() + experimentID + "_sim_cases_" + formattedParamID + ".csv";
            System.out.println("OutputFolder: " + ep.getOutputFolder() + ", fCaseList: " + fCaseList);

            // Set up vaccination campaigns and run the epidemic
            mp.setVaccinationCampaigns(ep.getStaticFilesFolder(), ep.getfCampaigns());
            String fIncursions = ep.getfIncursions();

            // Set up an epidemic
            Epidemic epi = new Epidemic(mp, ep.getMaxCases(), ep.getStartInDays(), ep.getEndInDays(), epiModel, mvModel, dm,
                    ep.getInputFolder(), ep.getStaticFilesFolder(), ep.getfInitialCases(), fIncursions, ep.getAllowEndogenousToTransmit(), ep.getAdditionalString(),
                    fByPopCoverageOutput, fMixedPopCoverageOutput, formattedParamID, random);

            // Run the epidemic, to obtain epidemic outcomes
            eo = epi.run(testRun, stopIfExtinct, ep.getAdditionalString());

            // Write out caseList and summary of epidemic outcomes
            System.out.println("Writing cases to: " + fCaseList);
            CaseListReader.writeCsvFile(fCaseList, eo.getCaseList());
            eoWriter.appendLine(eo.getValues(includeTicToc));

            // Write out population sizes
            if (folderFinalPops.length() > 0 ) {
                System.out.println("Going to write out population ...");
                CSVWriter popWriter = new CSVWriter(folderFinalPops + "final_pops_" +  formattedParamID + ".csv");
                popWriter.appendLine(mp.getMixedPop().getPopHeader()); // popHeader same for all populations
                popWriter.appendLine(mp.toStringPopns());
                popWriter.close();
            }


        } // ------------------------ End main simulation loop ---------------------
        eoWriter.close();
        System.out.println("Completed all parameters and runs. Writing EpidemicOutcomes to " + fEpidemicOutcomes);

    }

}