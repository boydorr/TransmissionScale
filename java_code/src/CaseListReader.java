/**
 * Created by rebeccamancy on 30/08/2015.
 */


import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Scanner;

public class CaseListReader {

    // Delimiter used in CSV file
    private static final String COMMA_DELIMITER = ",";
    private static final String NEW_LINE_SEPARATOR = "\n";

    // CSV file header
    private static final String FILE_HEADER = Case.toStringCsvHeader();

    /**
     * Writes a csv file containing a caseList
     * @param fileName
     * @param caseList
     */
    public static void writeCsvFile(String fileName, ArrayList<Case> caseList) {

        FileWriter fileWriter = null;

        try {
            fileWriter = new FileWriter(fileName);

            // Write the CSV file header and new line
            fileWriter.append(FILE_HEADER.toString());
            fileWriter.append(NEW_LINE_SEPARATOR);

            // Write a new case object to the CSV file
            for (Case c : caseList) {
                fileWriter.append(c.toStringCsv());
                fileWriter.append(NEW_LINE_SEPARATOR);
            }
            // System.out.println("CSV file was created successfully !!!");

        } catch (Exception e) {
            System.out.println("Error in CsvFileWriter !!!");
            e.printStackTrace();

        } finally {
            try {
                fileWriter.flush();
                fileWriter.close();
            } catch (IOException e) {
                System.out.println("Error while flushing/closing fileWriter !!!");
                e.printStackTrace();
            }
        }
    }

    /**
     * Reads in CSV file of cases
     * @param fCases
     * @return
     */
    public static ArrayList<Case> readCSVFile(String fCases) {

        final String simCols = "caseID,parentID,popID,popName,dogCount,dogsE,dogsV," +
                "Th,Td,dogDensityK,dayGenerated,serialInterval,dayInfectious,infectiousPeriod," +
                "x_coord,y_coord,strainID,typeOfCase," +
                "nExcursions,nDistinctDogs,diedE,nAbandonments,nBitesE,nBitesV,nReinfections,nFailedTransmissions,nTransmissions," +
                "mpDogCount,mpDogsE,mpDogsV,nDistinctS,nDistinctE,nDistinctV";

        final String dataCols = "caseID,ParentID,popID,popName,dogCount,dogsE,dogsV,Th,Td,dogDensityK," +
                "dayGenerated,serialInterval,dayInfectious,infectiousPeriod," +
                "x_coord,y_coord," +
                "strainID,typeOfCase," +
                "nExcursions,nDistinctDogs,diedE,nAbandonments,nBitesE,nBitesV,nReinfections,nFailedTransmissions,nTransmissions," +
                "Symptoms.started,Month.symptoms.started,Quarter.symptoms.started";

        Boolean isSim = Boolean.FALSE;

        ArrayList<Case> cases = new ArrayList<Case>();

        if (fCases.length()>0) {

            final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces
            // Read in list of initial incubating cases
            Scanner sc = null;
            int ID, parentID, popID; String popName; double dayGenerated, serialInterval, dayInfectious, infectiousPeriod, x_coord, y_coord; int strainID; Boolean isExogenous; // what we'll be reading in
            // Initialise scanner for reading in
            try {
                sc = new Scanner(new File(fCases));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();
                header = header.replaceAll("\\s+",""); // strip spaces in header

                if (header.equals(simCols)) {
                    isSim = Boolean.TRUE;
                    System.out.println("CaseListReader > readCSVFileWithOutcomes > and reading in SIMULATED data with header: " + header);
                } else if (header.equals(dataCols)) {
                    isSim = Boolean.FALSE;
                    System.out.println("CaseListReader > readCSVFileWithOutcomes > and reading in OBSERVED data with header: " + header);
                } else {
                    System.err.println("CaseListReader > readCSVFile > " +  fCases + " header not as expected:" + header + "\nWas expecting:" + dataCols);
                    System.exit(0);
                }

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                while (sc.hasNext()) {

                    // Columns in csv: (case ids start from 1):  dayGenerated, serialInterval, dayInfectious, popID, x, y, id
                    ID = Integer.parseInt(sc.next());
                    parentID = Integer.parseInt(sc.next());
                    popID = Integer.parseInt(sc.next());
                    popName = sc.next();
                    String dogCount = sc.next(); // unused
                    String dogsE = sc.next(); // unused
                    String dogsV = sc.next(); // unused
                    String Th = sc.next(); // unused
                    String Td = sc.next(); // unused
                    String dogDensityK = sc.next(); // unused
                    dayGenerated = Double.parseDouble(sc.next());
                    serialInterval = Double.parseDouble(sc.next());
                    dayInfectious = Double.parseDouble(sc.next());
                    infectiousPeriod = Double.parseDouble(sc.next());
                    x_coord = Double.parseDouble(sc.next());
                    y_coord = Double.parseDouble(sc.next());
                    strainID = Integer.parseInt(sc.next());
                    Case.caseType typeOfCase = Util.getEnumFromString(Case.caseType.class, sc.next());

                    // Throw away any extra info on this line
                    sc.useDelimiter(NEW_LINE_SEPARATOR);
                    sc.next();
                    sc.useDelimiter(CSV_DELIMITER);

                    // Add to the caselist
                    cases.add(new Case(ID, parentID, popID, popName, dayGenerated, serialInterval, dayInfectious, infectiousPeriod, x_coord, y_coord, strainID, typeOfCase));
                }

            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
                System.exit(1);
            } finally {
                if (sc != null) {
                    try {
                        sc.close();
                    }
                    catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(1);
                    }
                }
            }
        }
        return(cases);
    }

    /**
     * Reads in CSV file of cases, with inputFolder provided explicitly
     * @param inputFolder
     * @param fCases
     * @return
     */
    public static ArrayList<Case> readCSVFileWithOutcomes(String inputFolder, String fCases) {

        final String simCols = "caseID,parentID,popID,popName,dogCount,dogsE,dogsV," +
                "Th,Td,dogDensityK,dayGenerated,serialInterval,dayInfectious,infectiousPeriod," +
                "x_coord,y_coord,strainID,typeOfCase," +
                "nExcursions,nDistinctDogs,diedE,nAbandonments,nBitesE,nBitesV,nReinfections,nFailedTransmissions,nTransmissions," +
                "mpDogCount,mpDogsE,mpDogsV,nDistinctS,nDistinctE,nDistinctV";

        final String dataCols = "caseID,ParentID,popID,popName,dogCount,dogsE,dogsV,Th,Td,dogDensityK," +
                "dayGenerated,serialInterval,dayInfectious,infectiousPeriod," +
                "x_coord,y_coord," +
                "strainID,typeOfCase," +
                "nExcursions,nDistinctDogs,diedE,nAbandonments,nBitesE,nBitesV,nReinfections,nFailedTransmissions,nTransmissions," +
                "Symptoms.started,Month.symptoms.started,Quarter.symptoms.started";

        Boolean isSim = Boolean.FALSE;

        ArrayList<Case> cases = new ArrayList<Case>();

        System.out.println("In CaseListReader > readCSVFileWithOutcomes and Reading in from " + fCases);

        if (fCases.length()>0) {

            final String CSV_DELIMITER = ", *|\\n|\\n\\r";      // delimiters in csv file are new lines and commas + arbitrary num of spaces
            // Read in list of initial incubating cases
            Scanner sc = null;
            int ID, parentID, popID; String popName; double dayGenerated, serialInterval, dayInfectious, infectiousPeriod, x_coord, y_coord; int strainID; Boolean isExogenous; // what we'll be reading in
            int nExcursions, nDistinctDogs, nAbandonments, nBitesE, nBitesV, nReinfections, nFailedTransmissions, nTransmissions; Boolean diedE;
            int mpDogCount, mpDogsE, mpDogsV, nDistinctS = -1, nDistinctE = -1, nDistinctV = -1;
            // Initialise scanner for reading in
            try {
                sc = new Scanner(new File(inputFolder + fCases));
                sc.useDelimiter(NEW_LINE_SEPARATOR);   // delimiters in csv file are new lines and commas + arbitrary num of spaces
                String header = sc.next();
                header = header.replaceAll("\\s+",""); // strip spaces in header
                if (header.equals(simCols)) {
                    isSim = Boolean.TRUE;
                    System.out.println("CaseListReader > readCSVFileWithOutcomes > and reading in SIMULATED data with header: " + header);
                } else if (header.equals(dataCols)) {
                    isSim = Boolean.FALSE;
                    System.out.println("CaseListReader > readCSVFileWithOutcomes > and reading in OBSERVED data with header: " + header);
                } else {
                    System.err.println("CaseListReader > readCSVFile > " +  fCases + " header not as expected:" + header + "\nWas expecting:" + dataCols);
                    System.exit(0);
                }

                // Now read in items separated by commas
                sc.useDelimiter(CSV_DELIMITER);   // delimiters in csv file are new lines and commas + arbitrary num of spaces

                int counter = 0;
                while (sc.hasNext()) {

                    // Columns in csv: (case ids start from 1):  dayGenerated, serialInterval, dayInfectious, popID, x, y, id
                    ID = Integer.parseInt(sc.next());
                    parentID = Integer.parseInt(sc.next());
                    popID = Integer.parseInt(sc.next());
                    popName = sc.next();
                    String dogCount = sc.next(); // unused
                    String dogsE = sc.next(); // unused
                    String dogsV = sc.next(); // unused
                    String Th = sc.next(); // unused
                    String Td = sc.next(); // unused
                    String dogDensityK = sc.next(); // unused
                    dayGenerated = Double.parseDouble(sc.next());
                    serialInterval = Double.parseDouble(sc.next());
                    dayInfectious = Double.parseDouble(sc.next());
                    infectiousPeriod = Double.parseDouble(sc.next());
                    ///System.out.println("CaseListReader > " + ID + " " + dogCount + " " + dogsV + " " + Th);
                    x_coord = Double.parseDouble(sc.next());
                    y_coord = Double.parseDouble(sc.next());
                    strainID = Integer.parseInt(sc.next());
                    //isExogenous = Boolean.parseBoolean(sc.next());
                    Case.caseType typeOfCase = Util.getEnumFromString(Case.caseType.class, sc.next());

                    // Parts generated as CaseOutcomes
                    nExcursions = Integer.parseInt(sc.next());
                    nDistinctDogs = Integer.parseInt(sc.next());
                    /// System.err.println("In CaseListReader > and number of distinct dogs = " + nDistinctDogs);
                    diedE = Boolean.parseBoolean(sc.next());
                    nAbandonments = Integer.parseInt(sc.next());
                    nBitesE = Integer.parseInt(sc.next());
                    nBitesV = Integer.parseInt(sc.next());
                    nReinfections = Integer.parseInt(sc.next());
                    nFailedTransmissions = Integer.parseInt(sc.next());
                    nTransmissions = Integer.parseInt(sc.next());

                    if (isSim) {
                        mpDogCount = Integer.parseInt(sc.next());
                        mpDogsE = Integer.parseInt(sc.next());
                        mpDogsV = Integer.parseInt(sc.next());
                        nDistinctS = Integer.parseInt(sc.next());
                        nDistinctE = Integer.parseInt(sc.next());
                        nDistinctV = Integer.parseInt(sc.next());
                    }

                    // Throw away any extra info on this line
                    if (!header.equals(simCols) & sc.hasNext()) { // not EOF
                        sc.useDelimiter(NEW_LINE_SEPARATOR);
                        String throwaway = sc.next();
                        /// System.err.println("For ID=" + ID + " throwaway value in CaseListReader: " + throwaway);
                        sc.useDelimiter(CSV_DELIMITER); // set back to CSV_DELIMITER for next line
                    }

                    /// System.out.println("In caseListReader > readCSVFileWithOutcomes and reading in entry " + counter + " with ID " + ID);
                    counter ++;

                    // Add to the caselist
                    Case c = new Case (ID, parentID, popID, popName, dayGenerated, serialInterval, dayInfectious, infectiousPeriod, x_coord, y_coord, strainID, typeOfCase);
                    CaseOutcomes co = new CaseOutcomes(nAbandonments, nBitesE, nBitesV, nTransmissions, nFailedTransmissions, nReinfections, diedE,
                            nDistinctDogs, nDistinctS, nDistinctE, nDistinctV);
                    c.setOutcomes(co);
                    cases.add(c);
                }

            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
                System.exit(1);
            } finally {
                if (sc != null) {
                    try {
                        sc.close();
                    }
                    catch (Exception ex) {
                        ex.printStackTrace();
                        System.exit(1);
                    }
                }
            }
        }
        return(cases);
    }

}


