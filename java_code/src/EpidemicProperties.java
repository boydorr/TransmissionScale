/**
 * Created by rebeccamancy on 15/06/2016.
 **/

import java.io.*;
import java.util.ArrayList;
import java.util.Properties;
import jdistlib.rng.RandomEngine;

public class EpidemicProperties {

    Properties p = new Properties();

    public int getMaxCases() { return Integer.parseInt(p.getProperty("maxCases")); }
    public double getStartInDays() { return Double.parseDouble(p.getProperty("startInDays"));}
    public double getEndInDays() {return Double.parseDouble(p.getProperty("endInDays"));}
    public double getSiMeanlog() {return Double.parseDouble(p.getProperty("siMeanlog"));}
    public double getSiSdlog() {return Double.parseDouble(p.getProperty("siSdlog"));}
    public double getIpMean() { return Double.parseDouble(p.getProperty("ipMean")); }
    public double getIpVar() { return Double.parseDouble(p.getProperty("ipVar")); }
    public double getTdMean() { return Double.parseDouble(p.getProperty("TdMean")); }
    public double getThShape() { return Double.parseDouble(p.getProperty("ThShape")); }
    public double getThScale() { return Double.parseDouble(p.getProperty("ThScale")); }
    public double getSkShape() { return Double.parseDouble(p.getProperty("skShape")); }
    public double getSkScale() { return Double.parseDouble(p.getProperty("skScale")); }
    public double getLongDistProb() { return Double.parseDouble(p.getProperty("longDistProb")); }
    public double getTransGivenBiteProb() { return Double.parseDouble(p.getProperty("transGivenBiteProb")); }
    public double getPerCapBirthRate() { return Double.parseDouble(p.getProperty("perCapBirthRate")); }
    public double getPerCapDeathRate() { return Double.parseDouble(p.getProperty("perCapDeathRate")); }
    public String getAreaName() { return p.getProperty("areaName"); }
    public String getepidemiologicalModelName() { return p.getProperty("epidemiologicalModelName"); }
    public String getMovementModelName() {return p.getProperty("movementModelName"); }
    public String getInputFolder() { return p.getProperty("inputFolder"); }
    public String getOutputFolder() { return p.getProperty("outputFolder"); }
    public String getStaticFilesFolder() { return p.getProperty("staticFilesFolder"); }
    public String getfPops() { return p.getProperty("fPops"); }
    public String getfWKT() { return p.getProperty("fWKT"); }
    public String getfInitialCases() { return p.getProperty("fInitialCases"); }
    public String getfIncursions() { return p.getProperty("fIncursions"); }
    public String getfCampaigns() { return p.getProperty("fCampaigns"); }
    public String getfEpiVariables() { return p.getProperty("fEpiVariables"); }
    public double getCellAreaSqKm() { return Double.parseDouble(p.getProperty("cellAreaSqKm")); }
    public String getInitialLocationsType() { return p.getProperty("initialLocationsType"); }
    public String getLongDistanceLocationsType() { return p.getProperty("longDistanceLocationsType"); }
    public Boolean getAllowEndogenousToTransmit() { return Boolean.parseBoolean(p.getProperty("allowEndogenousToTransmit")); }
    public String getAdditionalString() { return p.getProperty("additionalString"); }

    public void setfInitialCases(String fInitialCases) {
        p.put("fInitialCases", fInitialCases);
    }
    public void setfIncursions(String fIncursions) {
        p.put("fIncursions", fIncursions);
    }


    public EpidemicProperties(String fProperties) {

        p = readProperties(p, fProperties);
        System.out.println("In EpidemicProperties" + p.toString() + " " + fProperties);

    }

    /**
     * Used for constructing a new Properties file
     */
    public EpidemicProperties(String fProperties, String expID, String parametersFolder, String folderEpidemicOutcomes, String folderIncursionCases, String additionalString,
                              int paramID, int firstParamID, String epidemiologicalModelName, String movementModelName, String resolution, Boolean allowEndogenousToTransmit,
                              String fEpiVariables, String fCampaigns, ArrayList<ParamSet> paramSets, String home, double totalAreaSqKm, int maxCases,
                              RandomEngine random) {

        String cellAreaSqKm;

        if (resolution.equals("all")) {
            cellAreaSqKm = "" + totalAreaSqKm;
        } else if (resolution.equals("Data")) {
            cellAreaSqKm = "" + 1000 / 1000000;
        } else {
            cellAreaSqKm = "" + Double.parseDouble(resolution) * Double.parseDouble(resolution) / 1000000;
        }

        /// System.out.println("cellAreaSqKm: " + cellAreaSqKm);
        String formattedParamID = String.format("%03d", paramID);
        p = new Properties();

        // Distribution parameters, etc. [computed using Make_Input on 2017-05-22]
        p.put("fEpiVariables",fEpiVariables);
        p.put("epidemiologicalModelName", epidemiologicalModelName);  // Not used, but indicates that incubation and infectious periods are sampled from pre-computed values,
                                        // rather than simulated on the fly
        p.put("movementModelName", movementModelName);

        // Not used in manuscript because serial interval and infectious periods are drawn from the data
        p.put("siMeanlog", "2.9519653");
        p.put("siSdlog", "0.8291439");
        p.put("ipMean", "3.232078"); // Used to compute shape and scale for Gamma distribution of infectious periods
        p.put("ipVar", "2.799851");

        // Priors, etc.
        p.put("ThShape", Double.toString(paramSets.get(paramID-firstParamID).getThShape()));
        p.put("ThScale", Double.toString(paramSets.get(paramID-firstParamID).getThScale()));
        p.put("TdMean", Double.toString(paramSets.get(paramID-firstParamID).getTd()));

        // Other parameters
        p.put("cellAreaSqKm", cellAreaSqKm); // Area of the cell in square KILOMETRES; baseline is 1.0
        double skShape = 0.383985202913735, skScale = 57.6411419785632;
            // Weibull params, estimated on 2021-06-09, with estimated mean of 214m and sd of 720m
        p.put("skShape", Double.toString(skShape));
        p.put("skScale", Double.toString(skScale));

        p.put("transGivenBiteProb", "0.49"); // Baseline is 0.49
        p.put("longDistProb", "0.02");

        // Demographic parameters
        double daysPerMonth = 365.0/12.0;
        double bd = 1/(25.8*daysPerMonth); // Rate in days: 25.8 months is mean age at death in Czupryna et al. 2016
        String strBd = "" + bd;
        p.put("perCapBirthRate", strBd);
        p.put("perCapDeathRate", strBd);

        // Shapefiles, etc.
        p.put("inputFolder", parametersFolder);
        p.put("outputFolder", folderEpidemicOutcomes);
        p.put("staticFilesFolder", home + "static_files/"); // also the folder for fPops
        p.put("fPops", "pops_" + resolution + ".csv");
        p.put("fWKT", "grd_" + resolution + ".txt");
        p.put("allowEndogenousToTransmit", Boolean.toString(allowEndogenousToTransmit));
        p.put("additionalString", additionalString);

        String fIncursions = "incursions_" + formattedParamID + ".csv";
        p.put("fIncursions", fIncursions);
        p.put("fCampaigns", fCampaigns);
        
        // Simulation / Epidemic parameters
        p.put("maxCases", Integer.toString(maxCases));
        p.put("startInDays", "0");
        p.put("endInDays", "5112"); // 2002-01-01 to 2015-12-31 is 5112 days; from 1998-01-01 to 2002-01-01 is 1461 days

        p.put("initialLocationsType", "randPropK");  // Options are: randPropK, randUnif, randUnifAboveMedian, randPropLogK
        p.put("longDistanceLocationsType", "randPropK"); // For endogenous long-distance movement
        p.put("incursionLocationsType", "randPropK"); // For incursions

        System.out.println("About to write out EpidemicProperties after calling constructor: " + p);

        // Write out properties file
        writeProperties(fProperties);
    }

    /**
     * Writes out a properties file to disk, given a location
     * @param f file path of properties file
     */
    public void writeProperties(String f) {
        FileWriter fw = null;
        try {
            fw = new FileWriter(f);
            String test = "";
            p.store(fw, test);
        } catch (Exception e) {
            System.out.println("Error in EpidemicProperties > writeProperties !!!");
            e.printStackTrace();
        }
    }

    /**
     * Reads in a properties file
     * @param p Properties object (e.g. to be over-written)
     * @param f file path of properties file
     * @return properties object
     */
    private static Properties readProperties (Properties p, String f) {

        InputStream input = null;

        try {
            // Load the properties file
            input = new FileInputStream(f);
            p.load(input);

        } catch (IOException ex) {

        } finally {
            if (input!=null) {
                try {
                    input.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return p;
    }
}